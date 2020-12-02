import React from 'react'

import style from './LogoStyle.scss'

type PropsType = { }

export const Home: React.FC<PropsType> = props => {
    const logo = (
        <h1>Wordlist</h1>
    )

    return (
        <div className={ style.logo }>
            <span className={ style.home }>{ logo }</span>
        </div>
    )
}
