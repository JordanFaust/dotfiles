// Reply markdown drawn in Macchiato colours. Fenced code goes to the engine's
// Code element and tables to its Markdown element; everything else is drawn here.

import { theme } from './palette'

type Node = any
type Elements = { Box: any; Text: any; Link: any; Code: any; Markdown: any }

const md = theme.md

const FENCE = /^\s*```(\S*)/
const FENCE_END = /^\s*```\s*$/
const TABLE = /^\s*\|/
const HEADING = /^ {0,3}(#{1,6})\s+(.*)$/
const RULE = /^\s*([-*_])\s*(\1\s*){2,}$/
const QUOTE = /^\s*>/
const ITEM = /^(\s*)([-*+]|\d+[.)])\s+(.*)$/
// a line that starts a block other than a paragraph
const BLOCK_START = /^(\s*(```|\||>|([-*+]|\d+[.)])\s)| {0,3}#{1,6}\s)/

// `code` | **bold** | [label](href) | *italic* | _italic_
const INLINE = /(`[^`\n]+`)|(\*\*[^*\n]+\*\*)|(\[[^\]\n]+\]\([^)\s]+\))|(\*[^*\s][^*\n]*\*)|(\b_[^_\n]+_\b)/g

function spans(s: string, el: Elements, key: string): Node[] {
  const { Text, Link } = el
  const out: Node[] = []
  let at = 0
  let n = 0
  for (const m of s.matchAll(INLINE)) {
    const start = m.index ?? 0
    if (start > at) out.push(s.slice(at, start))
    const tok = m[0]
    const k = `${key}.${n++}`
    if (m[1]) {
      out.push(<Text key={k} color={md.code}>{tok.slice(1, -1)}</Text>)
    } else if (m[2]) {
      out.push(<Text key={k} bold color={md.bold}>{tok.slice(2, -2)}</Text>)
    } else if (m[3]) {
      const [, label = '', href = ''] = tok.match(/^\[([^\]]+)\]\(([^)]+)\)$/) ?? []
      const text = <Text key={k} color={md.link} underline>{label}</Text>
      out.push(/^(https?|file):/.test(href) ? <Link key={k} href={href}>{text}</Link> : text)
    } else {
      out.push(<Text key={k} italic color={md.italic}>{tok.slice(1, -1)}</Text>)
    }
    at = start + tok.length
  }
  if (at < s.length) out.push(s.slice(at))
  return out
}

export function renderMarkdown(src: string, el: Elements, tone?: string): Node {
  const { Box, Text, Code, Markdown } = el
  const lines = src.replace(/\s+$/, '').split('\n')
  const blocks: Node[] = []
  const push = (node: Node) =>
    blocks.push(
      <Box key={`b${blocks.length}`} marginTop={blocks.length ? 1 : 0} flexDirection="column">
        {node}
      </Box>,
    )
  const line = (i: number) => lines[i] ?? ''

  let i = 0
  while (i < lines.length) {
    const l = line(i)

    if (!l.trim()) {
      i++
      continue
    }

    const fence = l.match(FENCE)
    if (fence) {
      const body: string[] = []
      i++
      while (i < lines.length && !FENCE_END.test(line(i))) body.push(line(i++))
      i++
      push(<Code source={body.join('\n')} language={fence[1] || undefined} />)
      continue
    }

    if (TABLE.test(l)) {
      const rows: string[] = []
      while (i < lines.length && TABLE.test(line(i))) rows.push(line(i++))
      push(<Markdown text={rows.join('\n')} />)
      continue
    }

    const heading = l.match(HEADING)
    if (heading) {
      push(<Text bold color={md.heading}>{spans(heading[2] ?? '', el, `h${i}`)}</Text>)
      i++
      continue
    }

    if (RULE.test(l)) {
      push(<Text color={md.rule}>{'─'.repeat(40)}</Text>)
      i++
      continue
    }

    if (QUOTE.test(l)) {
      const q: string[] = []
      while (i < lines.length && QUOTE.test(line(i))) q.push(line(i++).replace(/^\s*>\s?/, ''))
      // prose gets a bar; a quote holding lists, headings, code or blank lines is
      // markdown again, framed in the quote colour
      const structured = q.length > 1 && q.some(s => !s.trim() || /^(\s*([-*+]|\d+[.)])\s| {0,3}#{1,6}\s|\s*```)/.test(s))
      push(
        structured ? (
          <Box borderStyle="round" borderColor={md.quote} paddingX={1} flexDirection="column">
            {renderMarkdown(q.join('\n'), el, md.quote)}
          </Box>
        ) : (
          <Box flexDirection="row">
            <Text color={md.quote}>│ </Text>
            <Text color={md.quote}>{spans(q.map(s => s.trim()).join(' '), el, `q${i}`)}</Text>
          </Box>
        ),
      )
      continue
    }

    if (ITEM.test(l)) {
      const items: Node[] = []
      let m: RegExpMatchArray | null
      while (i < lines.length && (m = line(i).match(ITEM))) {
        const depth = Math.floor((m[1] ?? '').length / 2)
        const marker = /\d/.test(m[2] ?? '') ? (m[2] ?? '') : '•'
        let text = m[3] ?? ''
        i++
        // indented continuation lines belong to the item
        while (i < lines.length && /^\s{2,}\S/.test(line(i)) && !ITEM.test(line(i))) text += ' ' + line(i++).trim()
        items.push(
          <Box key={`li${i}`} flexDirection="row" paddingLeft={depth * 2}>
            <Box width={marker.length + 1} flexShrink={0}>
              <Text color={md.bullet}>{marker}</Text>
            </Box>
            <Box flexGrow={1} flexShrink={1}>
              <Text color={tone}>{spans(text, el, `li${i}`)}</Text>
            </Box>
          </Box>,
        )
      }
      push(<Box flexDirection="column">{items}</Box>)
      continue
    }

    // paragraph: always takes its first line, so a line nothing else claims
    // cannot stall the loop
    const para: string[] = [l.trim()]
    i++
    while (i < lines.length && line(i).trim() && !BLOCK_START.test(line(i))) para.push(line(i++).trim())
    push(<Text color={tone}>{spans(para.join(' '), el, `p${i}`)}</Text>)
  }

  return <Box flexDirection="column">{blocks}</Box>
}
