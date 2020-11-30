import React from 'react'

import style from './IconStyle.scss'

type DeletePropsType = {
    onClick: () => void
}

export const Delete: React.FC<DeletePropsType> = props => {
    const onClick = (event: React.MouseEvent) => {
        event.preventDefault()
        event.stopPropagation()
        props.onClick()
    }

    const className = [
        'material-icons',
        style.i
    ].join(' ')

    return (
        <div className={ style.delete }>
            <i className={ className } onClick={ onClick }>
                clear
            </i>
        </div>
    )
}
