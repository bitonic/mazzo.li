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
import secrets
from datetime import datetime, timedelta
import stat
import sqlite3
import smtplib
import email.message

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
#   notifications_email: Optional[str], # if present, notify to this email on new replies
#   body: str,
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
    db_file: Path
    db: sqlite3.Connection
    posts_dir: Path
    comments_dir: Path
    operator_password: str
    smtp_password: str
    tokens: List[Token]

    def _init_db(self):
        self.db.row_factory = sqlite3.Row
        cur = self.db.cursor()
        cur.execute('pragma foreign_keys = ON')
        cur.execute('pragma journal_mode = WAL')
        cur.execute('''
            create table if not exists notifications_emails (
                post text not null,
                email text not null,
                primary key (post, email)
            ) strict
        ''')

    def __init__(self, *, posts_dir: str, comments_dir: str, db_file: str, operator_password: str, smtp_password: str):
        self.posts_dir = Path(posts_dir)
        self.comments_dir = Path(comments_dir)
        self.operator_password = operator_password
        self.smtp_password = smtp_password
        self.tokens = []
        self.db = sqlite3.connect(db_file)
        self._init_db()
        assert self.posts_dir.is_dir()
        assert self.comments_dir.is_dir()

    def posts(self):
        return set(map(lambda x: Path(x).stem, glob.glob(str(self.posts_dir / '*.html'))))

    def delete(self, post_id: str, req):
        password = validate_field(req, 'password', optional=True)
        if password != self.operator_password:
            raise AbortRequest(401, 'Not authorized.')
        comment_id = validate_field(req, 'comment_id', optional=False)
        def delete_comment(comments):
            new_comments = list(filter(lambda c: c['id'] != comment_id, comments))
            if len(new_comments) != len(comments)-1:
                raise AbortRequest(400, f'Could not find comment with id {comment_id}')
            return new_comments
        modify_comments(self.comments_dir, post_id, delete_comment)
        return []
    
    def _send_email(self, *, server: smtplib.SMTP_SSL, post: str, comment_id: str, to: str, operator_link: bool, operator_post: bool, comment):  
        by = ''
        if author := comment.get('author'):
            by = ' by ' + author
        if operator_post:
            by = 'by Francesco'
        msg = email.message.EmailMessage()
        msg['Subject'] = f'mazzo.li: new comment for post "{post}"{by}'
        msg['From'] = 'f@mazzo.li'
        msg['To'] = to
        msg.set_content(f"http://mazzo.li/posts/{post}.html{'?operator' if operator_link else ''}#comment-{comment_id}\n\n{comment['body']}")
        server.send_message(msg)

    def new(self, post_id: str, req):
        token = validate_field(req, 'token', optional=False)
        len_old_tokens = len(self.tokens)
        self.tokens = list(filter(lambda tk: tk.secret != token, self.tokens))
        if len_old_tokens-1 != len(self.tokens):
            raise AbortRequest(400, 'Bad token!')
        operator = False
        password = validate_field(req, 'password', optional=True)
        if password is not None:
            if password != self.operator_password:
                raise AbortRequest(401, 'Not authorized.')
            operator = True
        author = validate_field(req, 'author', optional=True)
        notifications_email = validate_field(req, 'notifications_email', optional=True)
        body = validate_field(req, 'body', optional=False)
        if not body:
            raise AbortRequest(400, 'Empty body')
        id = str(time.time_ns())
        comment = {
            'id': id,
            'time': datetime.now(timezone.utc).isoformat().replace("+00:00", "Z"),
            'operator': operator,
            'author': author,
            'body': body,
        }
        with smtplib.SMTP_SSL('smtp.fastmail.com', 465) as server:
            server.login('bitonic@fastmail.com', self.smtp_password)
            # Send operator email _before we add the comment_,
            # we don't want to proceed unless I know that something
            # is about to be posted.
            self._send_email(server=server, post=post_id, comment_id=id, to='f@mazzo.li', operator_post=operator, operator_link=True, comment=comment)
            # Add comment
            def append_to_comments(comments):
                comments.append(comment)
                return comments
            modify_comments(self.comments_dir, post_id, append_to_comments)
            cur = self.db.cursor()
            # Send non-operator emails (not including this one)
            cur.execute('select email from notifications_emails where post = ?', (post_id,))
            for row in cur.fetchall():
                self._send_email(server=server, post=post_id, comment_id=id, to=row['email'], operator_post=operator, operator_link=False, comment=comment)
            # Add email
            if notifications_email:
                cur.execute(
                    'insert or ignore into notifications_emails (post, email) values (?, ?)',
                    (post_id, notifications_email),
                )
                self.db.commit()
            # We're done
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
            raise AbortRequest(400, f'Post {repr(post_id)} not found')
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
