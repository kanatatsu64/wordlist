import React from 'react'

import { Card } from 'Types'
import { loadPlugin } from 'Plugin'

type PropsType = {
    card: Card
}

export const View: React.FC<PropsType> = props => {
    const { card } = props

    const [isUpside, setIsUpside] = React.useState(true)

    const { pluginid } = card
    const plugin = loadPlugin(pluginid)

    const onReverse = () => {
        setIsUpside(!isUpside)
    }

    const { ViewUpside, ViewDownside } = plugin

    return (
        <div onClick={ onReverse }>
            {isUpside ? (
                <ViewUpside card={ card }></ViewUpside>
            ) : (
                <ViewDownside card={ card }></ViewDownside>
            )}
        </div>
    )
}
