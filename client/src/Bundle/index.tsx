import React from 'react'
import ReactDOM from 'react-dom'

import { Uploader } from 'Bundle/Uploader'

const Main = (
    <Uploader />
)

window.onload = () => {
    const root = document.getElementById('csv-root')
    ReactDOM.render(Main, root)
}
