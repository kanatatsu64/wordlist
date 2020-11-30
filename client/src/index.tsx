import React from 'react'
import ReactDOM from 'react-dom'
import { BrowserRouter as Router, Switch, Route } from 'react-router-dom'

import { Center } from 'Lib/Align'
import { Home } from 'Logo'
import { TopPage } from 'Pages/TopPage'
import { CsvTablePage } from 'Pages/CsvTablePage'
import { CardPage } from 'Pages/CardPage'

const App = (
    <Router>
        <Center>
            <Home></Home>
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
        </Center>
    </Router>
)

window.onload = () => {
    const root = document.getElementById('root')
    ReactDOM.render(App, root)
}
