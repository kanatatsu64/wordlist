import React, { ReactElement } from 'react'
import ReactDOM from 'react-dom'
import { BrowserRouter as Router, Switch, Route, Link } from 'react-router-dom'
import Modal from 'react-modal'

import { Center } from 'Lib/Align'
import { Header } from 'Parts/Header'
import { TopPage } from 'Pages/TopPage'
import { BundlePageAsync, CardPageAsync, LearnPageAsync } from 'Pages/Async'
import style from './index.scss'
import './index.scss'

const App: React.FC<{}> = props => {
    const [menu, setMenu] = React.useState<ReactElement>(undefined)

    return (
        <Router>
            <Header>
                { menu }
            </Header>
            <Center className={ style.container }>
                <Switch>
                    <Route exact path="/">
                        <TopPage setMenu={ setMenu }></TopPage>
                    </Route>
                    <Route exact path="/card/:cardId">
                        <CardPageAsync setMenu={ setMenu }></CardPageAsync>
                    </Route>
                    <Route exact path="/bundle/:bundleId">
                        <BundlePageAsync setMenu={ setMenu }></BundlePageAsync>
                    </Route>
                    <Route exact path="/bundle/learn/:bundleId">
                        <LearnPageAsync setMenu={ setMenu }></LearnPageAsync>
                    </Route>
                </Switch>
            </Center>
        </Router>
    )
}

window.onload = () => {
    const root = document.getElementById('root')
    Modal.setAppElement(root)
    ReactDOM.render(<App></App>, root)
}
