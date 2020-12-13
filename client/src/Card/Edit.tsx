import React from 'react'

import { Card } from 'Types'
import { loadPlugin } from 'Plugin'
import { update } from 'Api/Card'

type PropsType = {
    card: Card,
    onUpdate?: (card: Card) => void,
    onUpdated?: (card: Card) => void
}

export const Edit: React.FC<PropsType> = props => {
    const { card } = props

    const { pluginid } = card
    const plugin = loadPlugin(pluginid)

    const { Edit } = plugin

    const onUpdate = async (_card: Card) => {
        if (props.onUpdate) props.onUpdate(card)
        await update(_card)
        if (props.onUpdated) props.onUpdated(_card)
    }

    return (
        <Edit card={ card } update={ onUpdate }></Edit>
    )
}
