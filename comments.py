from contextlib import contextmanager
from pathlib import Path
import json
import glob
import fcntl
import tempfile
from dataclasses import dataclass
from typing import Dict, Any, List
from datetime import datetime, timezone
import os
import time
import subprocess
import secrets
from datetime import datetime, timedelta
import stat

@contextmanager
def lock_comment_file(comments_dir: Path, post_id: str):
    # We open with `a` just to create the file if it's missing, but
    # not truncate it if it's not.
    with open(str(comments_dir / Path(post_id + '.json')), 'a+b') as f:
        f.seek(0)
        fcntl.lockf(f, fcntl.LOCK_EX)
        try:
            yield f
        finally:
            fcntl.lockf(f, fcntl.LOCK_UN)

def modify_comments(comments_dir: Path, post_id: str, modify_comments):
    with lock_comment_file(comments_dir, post_id) as comments_file:
        data = comments_file.read()
        if not data:
            comments = []
        else:
            comments = json.loads(data)
        comments = modify_comments(comments)
        tmp_fd, tmp_name = tempfile.mkstemp(prefix=post_id, dir=str(comments_dir))
        os.fchmod(tmp_fd, stat.S_IRUSR | stat.S_IWUSR | stat.S_IROTH)
        with os.fdopen(tmp_fd, mode='w', encoding='utf-8') as tmp:
            json.dump(comments, tmp, indent=2)
            os.fsync(tmp)
        os.rename(tmp_name, comments_file.name)
        
# # JSON files schema
#
# For each post X.html, we have an X.json with comments.
#
# The schema for X.json is:
#
# [{
#   id: str,
#   time: str,
#   operator: bool, # Whether I have made the comment
#   author: Optional[str],
#   link: Optional[str],
#   body: str,
# }]
#
# The comment is markdown, rendered and DOM sanitized on the
# website. These are served statically -- no python code is
# needed to _read_ comments.
#
# # API
#
# I intentionally do not use anything in the HTTP request apart
# from the body and the method, because I am lazy.
#
# Post delete:
# {
#   post_id: str,
#   tag: 'delete',
#   password: str,
#   comment_id: str,
# }
#
# New post:
# {
#   post_id: str,
#   tag: 'new',
#   password: Optional[str], # if present, it's the author posting
#   name: Optional[str],
#   link: Optional[str],
#   body: str,
#   parent: Optional[str],
#   token: str,
# }
#
# Token: { tag: 'token' } => { token: str }

def validate_field(req: Dict[str, Any], fld: str, optional: bool):
    v = req.get(fld)
    if v is None and not optional:
        raise AbortRequest(400, f'Missing field {repr(fld)}')
    if v is not None and not isinstance(v, str):
        raise AbortRequest(400, f'{repr(fld)} is not a string')
    return v

@dataclass
class AbortRequest(Exception):
    code: int
    msg: str

@dataclass
class Token:
    time: datetime
    secret: str

class App:
    posts_dir: Path
    comments_dir: Path
    password: str
    tokens: List[Token]

    def __init__(self, *, posts_dir: str, comments_dir: str, password: str, msmtp: str = 'msmtp'):
        self.posts_dir = Path(posts_dir)
        self.comments_dir = Path(comments_dir)
        self.password = password
        self.msmtp = msmtp
        self.tokens = []
        assert self.posts_dir.is_dir()
        assert self.comments_dir.is_dir()

    def posts(self):
        return set(map(lambda x: Path(x).stem, glob.glob(str(self.posts_dir / '*.html'))))

    def delete(self, post_id: str, req):
        password = validate_field(req, 'password', optional=True)
        if password != self.password:
            raise AbortRequest(401, 'Not authorized.')
        comment_id = validate_field(req, 'comment_id', optional=False)
        def delete_comment(comments):
            new_comments = list(filter(lambda c: c['id'] != comment_id, comments))
            if len(new_comments) != len(comments)-1:
                raise AbortRequest(400, f'Could not find comment with id {comment_id}')
            return new_comments
        modify_comments(self.comments_dir, post_id, delete_comment)
        return []
    
    def new(self, post_id: str, req):
        token = validate_field(req, 'token', optional=False)
        len_old_tokens = len(self.tokens)
        self.tokens = list(filter(lambda tk: tk.secret != token, self.tokens))
        if len_old_tokens-1 != len(self.tokens):
            raise AbortRequest(400, 'Bad token!')
        operator = False
        password = validate_field(req, 'password', optional=True)
        if password is not None:
            if password != self.password:
                raise AbortRequest(401, 'Not authorized.')
            operator = True
        author = validate_field(req, 'author', optional=True)
        link = validate_field(req, 'link', optional=True)
        body = validate_field(req, 'body', optional=False)
        if not body:
            raise AbortRequest(400, 'Empty body')
        id = str(time.time_ns())
        comment = {
            'id': id,
            'time': datetime.now(timezone.utc).isoformat().replace("+00:00", "Z"),
            'operator': operator,
            'author': author,
            'link': link,
            'body': body,
        }
        email = f'''Subject: New comment for post {post_id}

http://mazzo.li/posts/{post_id}.html?operator#comment-{id}

{json.dumps(comment, indent=4)}
'''
        subprocess.run(
            [self.msmtp, '-a', 'default', 'f@mazzo.li'],
            input=email.encode('ascii'),
        )
        def append_to_comments(comments):
            comments.append(comment)
            return comments
        modify_comments(self.comments_dir, post_id, append_to_comments)
        return { 'id': id }

    def remove_stale_tokens(self):
        to_delete = 0
        now = datetime.now()
        while to_delete < len(self.tokens) and (now - self.tokens[to_delete].time > timedelta(minutes=5)):
            to_delete += 1
        self.tokens = self.tokens[to_delete:]

    def generate_token(self):
        token = secrets.token_hex(20)
        self.tokens.append(Token(time=datetime.now(), secret=token))
        return token

    def handle(self, env):
        self.remove_stale_tokens()
        if env['REQUEST_METHOD'] != 'POST':
            raise AbortRequest(405, 'Only POST allowed.')
        max_req_len = 1 * 1000 * 10 # 10KB
        body = env['wsgi.input'].read(max_req_len)
        if len(body) >= max_req_len:
            raise AbortRequest(413, f'Maximum request length {max_req_len} bytes')
        try:
            req = json.loads(body)
        except json.JSONDecodeError:
            raise AbortRequest(400, 'Request body is not valid JSON.')
        if not isinstance(req, dict):
            raise AbortRequest(400, 'Request body is not a dictionary.')
        tag = req.get('tag')
        if tag == 'token':
            return self.generate_token()
        post_id = req.get('post_id', '')
        # This protects us against any kind of path traversal
        if post_id not in self.posts():
            raise AbortRequest(400, 'Post {repr(post_id)} not found')
        if tag == 'delete':
            return self.delete(post_id, req)
        if tag == 'new':
            return self.new(post_id, req)
        raise AbortRequest(400, 'Bad JSON')

    def __call__(self, env, start_response):
        try:
            result = self.handle(env)
        except AbortRequest as err:
            start_response(f'{err.code}', [('Content-Type', 'application/json')])
            return [json.dumps(err.msg).encode('ascii')]
        start_response('200 OK', [('Content-Type', 'application/json')])
        return [json.dumps(result).encode('ascii')]
