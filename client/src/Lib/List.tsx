import React, { ReactElement } from 'react'

import style from 'Lib/ListStyle.scss'
import { Sequence } from 'Lib/Sequence'

type PropsType = {
    count?: number
    children: ReactElement[]
}

export const List: React.FC<PropsType> = props => {
    const { count } = props

    return (
        <ul className={ style.list }>
            <Sequence count={ count }>
                { props.children }
            </Sequence>
        </ul>
    )
}
