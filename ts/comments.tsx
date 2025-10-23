import * as React from "react"
import * as ReactDOM from "react-dom/client";
import * as marked from "marked"
import { markedSmartypants } from "marked-smartypants";
import * as dompurify from "dompurify"
import * as katex from "katex"
import { create } from "zustand"

function katexify(text: string) {
  const katexOpts: katex.KatexOptions = {
    macros: [],
    throwOnError: false,
  }
  return text
    .replaceAll(
      /\$\$[^\$]*\$\$/g,
      (substr) =>
        katex.renderToString(
          substr.slice(2, -2),
          {
            ...katexOpts,
            displayMode: true,
          }
        )
    )
    .replaceAll(       
      /\$[^\$]*\$/g,
      (substr) =>
        katex.renderToString(
          substr.slice(1, -1),
          {
            ...katexOpts,
            displayMode: false,
          }
        )
    )
}

function impossible<A>(_: never): A {
  throw "Impossible"
}

function parsePost(): string {
  const url = new URL(document.URL)
  const match = url.pathname.match(/^\/posts\/([a-zA-Z0-9-]+)\.html$/)
  if (match === null) {
    throw "No post match!"
  }
  return match[1]
}

function localStorageKey(post: string, k: string): string {
  return `comments:${post}:${k}`
}

interface CommentData {
  id?: string,
  time: string,
  operator: boolean,
  author: string | null,
  body: string,
}

type CommentsData = CommentData[]

type Success<T> = {
  ok: true,
  value: T,
};
type Failure = {
  ok: false,
  error: React.ReactNode,
};
type Result<T> = Success<T> | Failure;

function success<T>(x: T): Success<T> {
  return { ok: true, value: x };
}
function failure(error: React.ReactNode): Failure {
  return { ok: false, error };
}

function statusFailure(what: string, resp: Response): Failure {
  return failure(<>
    {what}: <code>{resp.status} {resp.statusText}</code>
  </>);
}

async function fetchCommentsData(post: string): Promise<Result<CommentsData>> {
  const resp = await fetch(`/assets/comments/${post}.json`, { cache: 'no-cache' })
  if (resp.status === 404) {
    // no comments yet
    return success([]);
  }
  if (!resp.ok) {
    return statusFailure('Could not get fetch comments', resp);
  }
  const txt = await resp.text()
  return success(JSON.parse(txt));
}

interface NewPost {
  password?: string,
  author?: string,
  notifications_email?: string,
  body: string,
}

async function newComment(post: string, data: NewPost): Promise<Result<{ id: string }>> {
  const tokenResp = await fetch(
    "/comments-api",
    {
      method: "POST",
      body: JSON.stringify({ tag: "token" }),
    }
  )
  if (!tokenResp.ok) {
    return statusFailure('Could not submit post', tokenResp);
  }
  const token = JSON.parse(await tokenResp.text())
  const body = {...data, post_id: post, tag: "new", token }
  if (!body["password"]) {
    delete body["password"];
  }
  const resp = await fetch(
    "/comments-api",
    {
      method: "POST",
      body: JSON.stringify(body),
    }
  )
  if (!resp.ok) {
    if (resp.status == 422) {
      return failure(<>
        Your post was detected as spam. Please <a href="mailto:f@mazzo.li">email me</a> if this was a mistake.
      </>)
    }
    return statusFailure('Could not add new post', resp);
  }
  return JSON.parse(await resp.text())
}

async function deleteComment(post: string, id: string, password: string): Promise<Result<void>> {
  const resp = await fetch(
    "/comments-api",
    {
      method: "POST",
      body: JSON.stringify({
        post_id: post,
        tag: "delete",
        comment_id: id,
        password: password,
      })
    }
  )
  if (!resp.ok) {
    return statusFailure('Could not delete post', resp);
  }
  return success(undefined);
}

const Comment: React.FunctionComponent<CommentData & { post: string, inCommentList?: boolean, isOperator: boolean, refreshComments: () => void, scrollToTextarea?: () => void }> = (props) => {
  const { inCommentList, isOperator, refreshComments, scrollToTextarea, post, ...comment } = props
  const bodyRef = React.useRef<HTMLDivElement>(null);
  React.useEffect(() => {
    if (bodyRef.current === null) { return; }
    let html = marked.marked.parse(katexify(comment.body));
    html = dompurify.sanitize(
      html,
      {
        ALLOWED_TAGS: ["em", "strong", "ul", "li", "ol", "code", "pre", "blockquote", "p", "a", "span"]
      }
    );
    bodyRef.current.innerHTML = html
    if (comment.id) {
      const hash = new URL(document.URL).hash
      if (hash == `#${commentAnchor(comment.id)}`) {
        bodyRef.current.scrollIntoView();
      }  
    }
  })
  const prefixBody = useDraftStore((state) => state.prefixBody)
  const date = new Date(comment.time)
  const twoDigits = (n: number) => (n > 9 ? "" : "0") + n.toString()
  const dateStr = `${date.getFullYear()}-${twoDigits(date.getMonth()+1)}-${twoDigits(date.getDate()+1)}`
  const reply : React.MouseEventHandler<HTMLAnchorElement> = (ev) => {
    ev.preventDefault();
    const author = comment.operator ? "**Francesco**, " : (comment.author ? `${comment.author}, ` : "Anonymous, ")
    let prefix = `> *${author}${dateStr}:*\n>\n`
    for (const line of comment.body.split("\n")) {
      prefix += `> ${line}\n`
    }
    prefix += "\n"
    prefixBody(post, prefix)
    scrollToTextarea && scrollToTextarea()
  }
  const author = comment.operator ?
    <span className="author operator">Francesco</span> :
    (comment.author ? <span className="author">{comment.author}</span> : null)
  const doDeleteComment: React.MouseEventHandler<HTMLAnchorElement> = (ev) => {
    ev.preventDefault();
    const password = prompt("Password")!;
    (async () => {
      await deleteComment(post, comment.id!, password)
      await refreshComments()
    })();
  }
  return <>
    <div>
      <div className="comment" id={commentAnchor(comment.id || "preview")} ref={bodyRef}></div>
    </div>
    <div
      className="footnotes footnotes-end-of-block"
      style={{
        padding: "0 0.25rem",
        borderTop: "none",
        fontSize: "1rem",
      }}
    >
      {author} <span className='comment-date'>{dateStr}</span> <br/>
      {comment.id &&
        <small style={{marginBottom: "0.5rem", display: "block"}}>
          <a href="#" onClick={reply}>Reply</a>
          {" "}
          <a href={`#${commentAnchor(comment.id)}`}>Permalink</a>
          {isOperator && <>
            {" "}
            <a href="#" onClick={doDeleteComment}>Delete</a>
          </>}
        </small>}
    </div>
    {inCommentList && <hr style={{marginBottom: "0.5rem"}} />}
  </>
}

interface DraftState {
  author: string,
  setAuthor: (post: string, author: string) => void,
  notificationsEmail: string,
  setNotificationsEmail: (post: string, link: string) => void,
  body: string,
  setBody: (post: string, body: string) => void,
  prefixBody: (post: string, prefix: string) => void,
  password: string,
  setPassword: (post: string, password: string) => void,
}

const useDraftStore = create<DraftState>((set) => ({
  author: "",
  setAuthor: (post: string, author: string) => set(() => {
    localStorage.setItem(localStorageKey(post, "author"), author)
    return { author }
  }),
  notificationsEmail: "",
  setNotificationsEmail: (post: string, notificationsEmail: string) => set(() => {
    localStorage.setItem(localStorageKey(post, "notificationsEmail"), notificationsEmail)
    return { notificationsEmail }
  }),
  body: "",
  setBody: (post: string, body: string) => set(() => {
    localStorage.setItem(localStorageKey(post, "body"), body)
    return { body }
  }),
  prefixBody: (post, prefix: string) => set((state) => {
    const body = prefix + state.body
    localStorage.setItem(localStorageKey(post, "body"), body)
    return { body }
  }),
  password: "",
  setPassword: (post, password: string) => {
    return set(() => ({ password }));
  }
}))

interface SubmitState {
  editing: boolean,
  status: "disarmed" | "armed" | "submitting",
  error?: React.ReactNode,
}

function commentAnchor(id: string): string {
  return `comment-${id}`
}

const Submit = React.forwardRef<HTMLTextAreaElement, { refreshComments: () => void, operator: boolean, post: string }>(({refreshComments, operator, post}, ref) => {
  const draft = useDraftStore((state) => ({...state}))
  const [state, setState] = React.useState<SubmitState>({
    editing: true,
    status: "disarmed",
  })
  React.useEffect(
    () => {
      draft.setAuthor(post, localStorage.getItem(localStorageKey(post, "author")) || "")
      draft.setNotificationsEmail(post, localStorage.getItem(localStorageKey(post, "notificationsEmail")) || "")
      draft.setBody(post, localStorage.getItem(localStorageKey(post, "body")) || "")
    },
    [post]
  )
  const setKey = (set: "setAuthor" | "setNotificationsEmail" | "setBody" | "setPassword"): React.ChangeEventHandler<HTMLInputElement | HTMLTextAreaElement> => (ev) => {
    draft[set](post, ev.target.value)
  }
  const onSubmit: React.FormEventHandler<HTMLFormElement> = (ev) => {
    ev.preventDefault();
    if (state.status === "disarmed") {
      setState((state) => { return { ...state, status: "armed" }})
      return
    } else if (state.status === "armed") {
      setState((state) => { return { ...state, status: "submitting" }});
      (async () => {
        const data = {
          author: draft.author,
          body: draft.body,
          password: draft.password,
          notifications_email: draft.notificationsEmail,
        }
        const resp = await newComment(post, { ...data })
        if (resp.ok) {
          draft.setBody(post, "")
          setState((state) => { return { ...state, editing: true, status: "disarmed" }})
          location.hash = `#${commentAnchor(resp.value.id)}`
          await refreshComments();
        } else {
          setState((state) => { return { ...state, error: resp.error }});
        }
      })();
    } else if (state.status === "submitting") {
      // nothing to do            
    } else {
      impossible(state.status)
    }
  }
  const disarm: React.FocusEventHandler<HTMLFormElement> = (ev) => {
    if (!ev.currentTarget.contains(ev.relatedTarget)) {
      setState(state => {
        return {...state, status: "disarmed" }
      })
    }
  }
  return <>
    {!state.editing && !state.error && <Comment
      refreshComments={refreshComments}
      post={post}
      operator={operator}
      author={draft.author}
      body={draft.body}
      time={new Date().toISOString()}
      isOperator={false}
    />}
    {state.error ?
      <span style={{color: "rgba(196, 4, 4, 1)", fontWeight: "bold", fontSize: "1.1em"}}>{state.error}</span> :
      <form
        className="submit"
        style={{
          display: "grid",
          gridTemplateColumns: "50% 50%",
          gridTemplateRows: "1.4rem auto 1.4rem",
          rowGap: "0.25rem",
          columnGap: "0.25rem",
          marginBottom: "0.5rem",
        }}
        onSubmit={onSubmit}
        onBlur={disarm}
        tabIndex={1}
      >
        {state.editing && <>
          {operator ?
            <input
              placeholder='Password'
              type='password'
              style={{
                gridColumnStart: "1",
                gridColumnEnd: "3",
                fontSize: "0.9rem",
                padding: "0 0.25rem",
              }}
              onChange={setKey("setPassword")}
              value={draft.password}
            /> :
            <>
              <input
                id="author-name" placeholder='Name'
                style={{
                  gridColumnStart: "1",
                  gridColumnEnd: "2",
                  fontSize: "0.9rem",
                  padding: "0 0.25rem",
                }}
                onChange={setKey("setAuthor")}
                value={draft.author}
              />
              <input
                id="author-link"
                placeholder='Email for reply notifications'
                style={{
                  gridColumnStart: "2",
                  gridColumnEnd: "3",
                  fontSize: "0.9rem",
                  padding: "0 0.25rem",
                }}
                onChange={setKey("setNotificationsEmail")}
                value={draft.notificationsEmail}
              />
            </>
          }
          <textarea
            id="comment-body"
            style={{
              gridColumnStart: "1",
              gridColumnEnd: "3",
              resize: "vertical",
              fontSize: "0.9rem",
            }}
            rows={7}
            value={draft.body}
            onChange={setKey("setBody")}
            ref={ref}
          />            
        </>}
        <button type="submit" disabled={!draft.body || state.status === "submitting"} style={{ fontSize: "0.9rem" }}>
          {
            state.status === "disarmed" ?
            "Submit" :
            state.status === "armed" ?
            <strong>Are you sure?</strong> :
            state.status === "submitting" ?
            "Submitting..." :
            impossible<string>(state.status)
          }
        </button>
        <button
          disabled={!draft.body}
          onClick={(ev) => {
            ev.preventDefault();
            setState(state => {
              return {...state, editing: !state.editing}
            })
          }}
          style={{ fontSize: "0.9rem" }}
        >
          {state.editing ? "Preview" : "Edit"}
        </button>
      </form>}
    {(state.editing || state.error) &&
      <div
        className="footnotes footnotes-end-of-block"
        style={{
          padding: "0 0.25rem",
          borderTop: "none",
        }}
      >
        <p>
          Providing a name is optional. You can also provide an email to get notifications on replies. If you do provide an email, it won't be publicly visible.
        </p>
        <p>
          Comments cannot be edited or deleted by you after submission, <a href="mailto:f@mazzo.li">email me</a> if you need to do so.
        </p>
        <p>
          The comment will be rendered using a limited <a href="https://en.wikipedia.org/wiki/Markdown">Markdown</a>. You can input math by using <code>$inline$</code> or <code>$$block$$</code> LaTeX syntax.
        </p>
      </div>}
  </>
})

const Comments: React.FunctionComponent<{
  operator: boolean,
  post: string,
}> = (props) => {
  const { post } = props
  const [comments, setComments] = React.useState<{
    loading: boolean,
    error?: React.ReactNode,
    firstLoaded: boolean,
    comments: CommentsData,
  }>({ loading: false, comments: [], firstLoaded: false })
  const loadComments = React.useCallback(
    () => {
      setComments((state) => { return { ...state, loading: true }});
      (async () => {
        const comments = await fetchCommentsData(post)
        if (comments.ok) {
          setComments((state) => { return { ...state, loading: false, firstLoaded: true, comments: comments.value }});
        } else {
          setComments((state) => { return { ...state, error: comments.error }});
        }
      })()
    },
    [post, setComments]
  )
  React.useEffect(loadComments, [loadComments])
  const textareaRef = React.useRef<HTMLTextAreaElement>(null)
  const scrollToTextarea = React.useCallback(
    () => {
      if (textareaRef.current) {
        textareaRef.current.scrollIntoView()
      }
    },
    [textareaRef]
  )
  return <>
    <hr/>
    <h3 style={{
      fontStyle: "italic",
      marginTop: "0",
    }}>
      Comments
      {comments.loading && !comments.error &&
        <span style={{color: "rgba(0,0,0,0.5)"}}> {comments.firstLoaded ? "(refreshing...)" : "(loading...)"}</span>}
    </h3>
    {comments.error ?
      <span style={{color: "rgba(196, 4, 4, 1)", fontWeight: "bold", fontSize: "1.1em"}}>{comments.error}</span> :
      <>
        {comments.comments.map(comment => <Comment post={post} refreshComments={loadComments} key={comment.id} inCommentList={true} isOperator={props.operator} scrollToTextarea={scrollToTextarea} {...comment} />)}
        {comments.firstLoaded &&
          <Submit ref={textareaRef} refreshComments={loadComments} {...props} />}
      </>}
  </>
}

export function run(container: HTMLElement) {
  marked.marked.use(markedSmartypants());
  const operator = new URL(document.URL).searchParams.has("operator")
  const post = parsePost()
  const root = ReactDOM.createRoot(container);
  root.render(<Comments operator={operator} post={post} />)
}