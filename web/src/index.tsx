import React from 'react'
import { render } from 'react-dom'
import './index.css'
import { App } from './components/App'

const getEnvironment = () => {
    if (process.env.NODE_ENV == 'production') {
        return {
            apiURL: 'localhost:8080',
            environment: 'production',
        }
    }
    return {
        apiURL: 'localhost:8080',
        environment: 'development',
    }
}

render(
    <React.StrictMode>
        <App env={getEnvironment()} />
    </React.StrictMode>,
    document.getElementById('root') as HTMLElement,
)
