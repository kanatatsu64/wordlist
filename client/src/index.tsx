import React from 'react'
import ReactDOM from 'react-dom'
import { BrowserRouter as Router, Switch, Route, Link } from 'react-router-dom'

import { Center } from 'Lib/Align'
import { Home } from 'Lib/Logo'
import { TopPage } from 'Pages/TopPage'
import { BundleTablePageAsync, CardPageAsync } from 'Pages/Async'

const App = (
    <Router>
        <Center>
            <Link to="/">
                <Home></Home>
            </Link>
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
