import React from 'react'

import style from './ListItemStyle.scss'

type PropsType = { }

export const ListItem: React.FC<PropsType> = props => {
    return (
        <div className={ style.item }>
            { props.children }
        </div>
    )
}
