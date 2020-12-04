import React from 'react'
import { useParams } from 'react-router-dom'

import { Bundle, Card } from 'Types'
import { getByName } from 'Api/Bundle'
import { View } from 'Card/View'

type PropsType = { }

export const CardPage : React.FC<PropsType> = props => {
    const { name } = useParams()

    const [bundle, setBundle] = React.useState<Bundle>(undefined)
    const size = bundle?.cards.length
    const [index, setIndex] = React.useState(0)
    const card = bundle?.cards[index]

    React.useEffect(() => {
        (async () => {
            setBundle(await getByName(name))
            setIndex(0)
        })()
    }, [name])

    const next = () => {
        setIndex((index + 1)%size)
    }

    const onNotRemembered = () => {
        const { word } = card
        alert(`${word} has yet to be remembered`)
        next()
    }
    const onRemembered = () => {
        const { word } = card
        alert(`${word} is remembered`)
        next()
    }

    const attrs = {
        onLeftClick: onNotRemembered,
        onRightClick: onRemembered
    }

    const viewCard = (card: Card) => (
        <div>
            <View card={ card } key={ card.cardid } { ...attrs } ></View>
        </div>
    )
    const loading = (
        <div>
            <span>loading ...</span>
        </div>
    )

    return (
        <div>
            <h1>{ name }</h1>
            { !!card ? viewCard(card) : loading }
        </div>
    )
}
