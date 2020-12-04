import React from 'react'
import { Link } from 'react-router-dom'

import { Home } from 'Lib/Logo'
import style from './HeaderStyle.scss'

type PropsType = {}

export const Header: React.FC<PropsType> = props => (
    <div className={ style.header }>
        <Link to="/">
            <Home></Home>
        </Link>
    </div>
)
