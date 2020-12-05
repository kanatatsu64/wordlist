import React from 'react'

import { List } from 'Lib/List'

type PropsType<V> = {
    values: V[]
    keyGen: (value: V) => React.ReactText
    itemGen: (value: V) => React.ReactElement
    count?: number
}

export class FCList<V> extends React.Component<PropsType<V>, {}> {
    render() {
        const { values, keyGen, itemGen } = this.props
        const items = values.map(value => {
            const key = keyGen(value)
            const item = itemGen(value)
            return React.cloneElement( item, { key })
        })
        return (
            <List>
                { items }
            </List>
        )
    }
}
