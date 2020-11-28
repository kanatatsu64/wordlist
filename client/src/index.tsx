import React from 'react'
import ReactDOM from 'react-dom'

import { Row } from 'Lib/Row'
import { Link } from 'Lib/Link'

const Main = (
    <Row>
        <span>Welcome to wordlist...</span>
        <Link to="/csv/upload">upload CSV</Link>
        <Link to="/list">show available wordlists</Link>
    </Row>
)

window.onload = () => {
    const root = document.getElementById('root')
    ReactDOM.render(Main, root)
}
