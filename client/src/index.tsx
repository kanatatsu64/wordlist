import React from 'react'
import ReactDOM from 'react-dom'
import { BrowserRouter as Router, Switch, Route, Link } from 'react-router-dom'

import { Center } from 'Lib/Align'
import { Header } from 'Parts/Header'
import { TopPage } from 'Pages/TopPage'
import { BundleTablePageAsync, CardPageAsync } from 'Pages/Async'
import './index.scss'

const App = (
    <Router>
        <Header></Header>
        <Center>
            <Switch>
                <Route exact path="/">
                    <TopPage></TopPage>
                </Route>
                <Route exact path="/bundle/table/:name">
                    <BundleTablePageAsync></BundleTablePageAsync>
                </Route>
                <Route exact path="/bundle/learn/:name">
                    <CardPageAsync></CardPageAsync>
                </Route>
            </Switch>
        </Center>
    </Router>
)

window.onload = () => {
    const root = document.getElementById('root')
    ReactDOM.render(App, root)
}
