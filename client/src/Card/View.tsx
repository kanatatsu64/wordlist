import React from 'react'

import { Card } from 'Api/Types'

type PropsType = {
    card: Card
}

export const View: React.FC<PropsType> = props => {
    const { card: { language, word, meaning }} = props

    const [isUpside, setIsUpside] = React.useState(true)

    const onReverse = () => {
        setIsUpside(!isUpside)
    }

    const upside = (
        <div>
            <span>{ language }</span>
            <span>{ word }</span>
        </div>
    )
    const downside = (
        <div>
            <span>{ meaning }</span>
        </div>
    )

    return (
        <div onClick={ onReverse }>
            { isUpside ? upside : downside }
        </div>
    )
}
