/* eslint-disable */
import { JSDOM } from 'jsdom'

// Setup JSDOM
const dom = new JSDOM('', {
    pretendToBeVisual: true,
})

// @ts-ignore
global.Event = dom.window.Event
// @ts-ignore
global.Node = dom.window.Node
// @ts-ignore
global.window = dom.window
// @ts-ignore
global.document = dom.window.document
// @ts-ignore
global.requestAnimationFrame = dom.window.requestAnimationFrame
