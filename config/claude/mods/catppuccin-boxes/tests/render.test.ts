import { test, expect } from 'claude-code/testing'

const reply = (text: string, surface = 'terminal') => ({
  surface, component: 'AssistantMessage', requestId: 'r', props: { text, isFirstOfReply: true },
}) as any

const prompt = (props: Record<string, unknown>) => ({
  surface: 'terminal', component: 'UserMessage', requestId: 'u',
  props: { isExpanded: true, origin: { kind: 'composer' }, ...props },
}) as any

test('reply is a raised card with Macchiato markdown', async ($, on) => {
  on('ui.render', { component: 'AssistantMessage' }, () => null as any)
  const json = JSON.stringify(await $.ui.render(reply('### Head\n\nUse `foo()` and **bold**.\n\n- item')))
  expect(json).toContain('#2a2e41') // raised card
  expect(json).toContain('#ed8796') // red heading, bullet
  expect(json).toContain('#a6da95') // green inline code
  expect(json).toContain('•')
})

test('prompt has a red bar on a rose tint', async ($, on) => {
  on('ui.render', { component: 'UserMessage' }, () => null as any)
  const json = JSON.stringify(await $.ui.render(prompt({ text: 'hello there' })))
  expect(json).toContain('#ed8796')
  expect(json).toContain('#3c3345')
  expect(json).not.toContain('▎')
  expect(json).toContain('hello there')
})

test('messages from other agents keep the engine drawing', async ($, on) => {
  on('ui.render', { component: 'UserMessage' }, ($, e) => {
    const { Text } = $.ui.resolve(e)
    return h(Text, null, 'engine-drawn') as any
  })
  const json = JSON.stringify(await $.ui.render(prompt({ text: 'hi', from: { kind: 'agent', name: 'x' } })))
  expect(json).toContain('engine-drawn')
  expect(json).not.toContain('#3c3345')
})

test('structured quotes are framed, prose quotes get a bar', async ($, on) => {
  on('ui.render', { component: 'AssistantMessage' }, () => null as any)
  const framed = JSON.stringify(await $.ui.render(reply('> **Open**\n>\n> 1. first\n> 2. second')))
  expect(framed).toContain('"round"')
  expect(framed).toContain('"1."')
  const bar = JSON.stringify(await $.ui.render(reply('> written over\n> two lines')))
  expect(bar).toContain('│ ')
  expect(bar).toContain('written over two lines')
})

test('indented "# " lines do not stall the renderer', { timeoutMs: 3000 }, async ($, on) => {
  on('ui.render', { component: 'AssistantMessage' }, () => null as any)
  const json = JSON.stringify(await $.ui.render(reply('Run:\n    # install deps\n    npm install\n\n  ## Indented heading')))
  expect(json).toContain('# install deps')
  expect(json).toContain('Indented heading')
})

test('engine rows between the boxes are inset to line up with them', async ($, on) => {
  on('ui.render', { component: 'TurnDuration' }, ($, e) => {
    const { Text } = $.ui.resolve(e)
    return h(Text, null, 'Worked for 3s') as any
  })
  const json = JSON.stringify(await $.ui.render({ surface: 'terminal', component: 'TurnDuration', requestId: 't', props: {} } as any))
  expect(json).toContain('Worked for 3s')
  expect(json).toContain('"marginX":2')
})
