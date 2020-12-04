import React from 'react'

import style from './LogoStyle.scss'

type PropsType = { }

export const Home: React.FC<PropsType> = props => {
    return (
        <div className={ style.logo }>
            <span className={ style.home }>Wordlist</span>
        </div>
    )
}
