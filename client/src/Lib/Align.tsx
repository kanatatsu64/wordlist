import React from 'react'

import { cons } from 'Utils'
import style from './AlignStyle.scss'

type PropsTyle = {
    className?: string
}

export const Center: React.FC<PropsTyle> = props => {
    const className = cons([props.className, style.center])
    return (
        <div className={ className }>
            { props.children }
        </div>
    )
}
