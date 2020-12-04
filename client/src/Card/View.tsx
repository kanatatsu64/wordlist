import React from 'react'

import { Card } from 'Types'
import { loadPlugin } from 'Plugin'
import style from './ViewStyle.scss'

type PropsType = {
    card: Card,
    onLeftClick: (card: Card) => void,
    onRightClick: (card: Card) => void
}

export const View: React.FC<PropsType> = props => {
    const { card } = props

    const [isUpside, setIsUpside] = React.useState(true)

    const { pluginid } = card
    const plugin = loadPlugin(pluginid)

    const onLeftClick = () => props.onLeftClick(card)

    const onRightClick = () => props.onRightClick(card)

    const onReverse = () => {
        setIsUpside(!isUpside)
    }

    const { ViewUpside, ViewDownside } = plugin

    const LeftArrow = <div className={ `${style.triangle} ${style.left}` }></div>
    const RightArrow = <div className={ `${style.triangle} ${style.right}` }></div>

    return (
        <div className={ style.container }>
            <div className={ style.arrow } onClick={ onLeftClick }>
                { LeftArrow }
            </div>
            <div className={ style.card } onClick={ onReverse }>
                {isUpside ? (
                    <ViewUpside card={ card }></ViewUpside>
                ) : (
                    <ViewDownside card={ card }></ViewDownside>
                )}
            </div>
            <div className={ style.arrow } onClick={ onRightClick }>
                { RightArrow }
            </div>
        </div>
    )
}
