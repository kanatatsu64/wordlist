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

    return (
        <div className={ style.delete }>
            <i className="material-icons" onClick={ onClick }>
                clear
            </i>
        </div>
    )
}

type AddPropsType = {
    onClick: () => void
}

export const Add: React.FC<AddPropsType> = props => {
    const onClick = (event: React.MouseEvent) => {
        event.preventDefault()
        event.stopPropagation()
        props.onClick()
    }

    return (
        <div className={ style.add }>
            <i className="material-icons" onClick={ onClick }>
                add_circle_outline
            </i>
        </div>
    )
}
