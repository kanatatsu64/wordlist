import React from 'react'
import { useHistory } from 'react-router-dom'

import style from './LogoStyle.scss'

type PropsType = { }

export const Home: React.FC<PropsType> = props => {
    const history = useHistory()

    const logo = (
        <h1>Wordlist</h1>
    )

    const onClick = () => {
        history.push('/')
    }

    return (
        <div className={ style.logo }>
            <span className={ style.home } onClick={ onClick }>{ logo }</span>
        </div>
    )
}
