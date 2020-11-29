import React from 'react'
import ReactDOM from 'react-dom'
import { BrowserRouter as Router, Switch, Route } from 'react-router-dom'

import { TopPage } from 'Pages/TopPage'
import { CsvTablePage } from 'Pages/CsvTablePage'
import { CardPage } from 'Pages/CardPage'

const App = (
    <Router>
        <Switch>
            <Route exact path="/">
                <TopPage></TopPage>
            </Route>
            <Route exact path="/csv/table/:name">
                <CsvTablePage></CsvTablePage>
            </Route>
            <Route exact path="/csv/learn/:name">
                <CardPage></CardPage>
            </Route>
        </Switch>
    </Router>
)

window.onload = () => {
    const root = document.getElementById('root')
    ReactDOM.render(App, root)
}
