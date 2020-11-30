import React from 'react'
import style from './FCListStyle.scss'

type PropsType<V> = {
    values: V[],
    keyGen: (value: V) => React.ReactText
    itemGen: (value: V) => React.ReactElement
}

export class FCList<V> extends React.Component<PropsType<V>, {}> {
    render() {
        const { values, keyGen, itemGen } = this.props
        return (
            <ul className={ style.list }>
                {values.map(value => {
                    const key = keyGen(value)
                    const item = itemGen(value)
                    return (
                        <li className={ style.item } key={ key }>{ item }</li>
                    )
                })}
            </ul>
        )
    }
}
