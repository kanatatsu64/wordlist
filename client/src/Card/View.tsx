import React from 'react'

import { Card } from 'Types'
import { loadPlugin } from 'Plugin'
import style from './ViewStyle.scss'

type PropsType = {
    card: Card,
    onLeftClick?: (card: Card) => void,
    onRightClick?: (card: Card) => void
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

    const LeftArrow = React.useMemo(() => {
        if (!props.onLeftClick) return
        const onClick = () => props.onLeftClick(card)

        return (
            <div className={ style.arrow } onClick={ onClick }>
                <div className={ `${style.triangle} ${style.left}` }></div>
            </div>
        )
    }, [card])

    const RightArrow = React.useMemo(() => {
        if (!props.onRightClick) return
        const onClick = () => props.onRightClick(card)

        return (
            <div className={ style.arrow } onClick={ onClick }>
                <div className={ `${style.triangle} ${style.right}` }></div>
            </div>
        )
    }, [card])

    return (
        <div className={ style.container }>
            { LeftArrow }
            <div className={ style.card } onClick={ onReverse }>
                {isUpside ? (
                    <ViewUpside card={ card }></ViewUpside>
                ) : (
                    <ViewDownside card={ card }></ViewDownside>
                )}
            </div>
            { RightArrow }
        </div>
    )
}
