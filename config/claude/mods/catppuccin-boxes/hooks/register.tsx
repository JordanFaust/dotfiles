import type { Register } from 'claude-code'

import { renderMarkdown } from './markdown'
import { theme } from './palette'

// The boxes sit in from the edges; this is how far
const INSET = 2

export const register: Register = on => {
  // Engine-drawn rows between the boxes, inset to line up with them
  const engineRows = ['ToolUse', 'ToolResult', 'ToolProgress', 'ToolGroup', 'CommandOutput', 'TurnDuration', 'InfoNotice', 'Spinner'] as const
  on('ui.render', { component: engineRows }, async ($, e, next) => {
    const drawn = await next(e)
    if (e.surface !== 'terminal' || !drawn) return drawn
    const { Box } = $.ui.resolve(e)
    return <Box marginX={INSET}>{drawn}</Box>
  })

  // Your prompts: a padded box. Task notifications and messages from other
  // agents keep the engine's drawing.
  on('ui.render', { component: 'UserMessage' }, ($, e, next) => {
    if (e.surface !== 'terminal' || e.props.task || e.props.from) return next(e)
    const { Box, Text } = $.ui.resolve(e)
    // the bar is a one-column strip of colour the row stretches to the box's height;
    // a column of glyphs instead leaks past the box when it can't be clipped
    return (
      <Box flexDirection="row" backgroundColor={theme.prompt.bg} marginTop={1} marginX={INSET}>
        <Box width={1} flexShrink={0} backgroundColor={theme.prompt.bar} />
        <Box flexGrow={1} flexShrink={1} paddingLeft={1} paddingRight={2} paddingY={1}>
          <Text color={theme.prompt.fg}>{e.props.text}</Text>
        </Box>
      </Box>
    )
  })

  // Claude's replies: a padded box with markdown in Macchiato colours
  on('ui.render', { component: 'AssistantMessage' }, ($, e, next) => {
    if (e.surface !== 'terminal') return next(e)
    const el = $.ui.resolve(e)
    return (
      <el.Box backgroundColor={theme.reply.bg} paddingX={2} paddingY={1} marginTop={1} marginX={INSET}>
        {renderMarkdown(e.props.text, el, theme.reply.fg)}
      </el.Box>
    )
  })
}
