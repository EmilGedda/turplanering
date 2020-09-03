import React from 'react'
import { render } from 'react-dom'
import { App } from './components/App'

const getEnvironment = () => {
    const browser = {
        hasTouch: 'ontouchstart' in window || navigator.maxTouchPoints > 0,
    }
    if (process.env.NODE_ENV == 'production') {
        return {
            apiURL: 'localhost:8080',
            environment: 'production',
            browser,
        }
    }
    return {
        apiURL: 'localhost:8080',
        environment: 'development',
        browser,
    }
}

render(
    <React.StrictMode>
        <App env={getEnvironment()} />
    </React.StrictMode>,
    document.getElementById('root') as HTMLElement
)
