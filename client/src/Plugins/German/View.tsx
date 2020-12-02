import React from 'react'

import { convert, shortenPart } from './Types'
import { ViewPropsType } from 'Plugin'

export const ViewUpside: React.FC<ViewPropsType> = props => {
    const card = convert(props.card)
    const { part, word } = card
    const part_ = shortenPart(part)

    return (
        <div>
            <span>{ part_ }</span>
            <span>{ word }</span>
        </div>
    )
}

export const ViewDownside: React.FC<ViewPropsType> = props => {
    const card = convert(props.card)
    const { meaning } = card

    return (
        <div>
            <span>{ meaning }</span>
        </div>
    )
}
