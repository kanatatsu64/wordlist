import React from 'react'

import style from './AlignStyle.scss'

export const Center: React.FC<{}> = props => {
    return (
        <div className={ style.center }>
            { props.children }
        </div>
    )
}
