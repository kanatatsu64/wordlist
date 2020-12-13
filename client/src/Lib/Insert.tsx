import React from 'react'

import style from './InsertStyle.scss'

type PropsType = {
    onClick: () => void
}

export const Insert: React.FC<PropsType> = props => {
    return (
        <div className={ style.container } onClick={ props.onClick }>
            <div className={ style.line }>
                <hr></hr>
            </div>
        </div>
    )
}
