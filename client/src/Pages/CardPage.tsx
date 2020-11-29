import React from 'react'
import { useParams } from 'react-router-dom'

import { Card } from 'Api/Types'
import { CSV, getByName } from 'Api/Csv'
import { View } from 'Card/View'

type PropsType = { }

export const CardPage : React.FC<PropsType> = props => {
    const { name } = useParams()

    const [csv, setCsv] = React.useState<CSV>(undefined)
    const size = React.useMemo(() => csv?.cards.length, [csv])
    const [index, setIndex] = React.useState(0)
    const card = React.useMemo(() => csv?.cards[index], [csv, index])

    React.useEffect(() => {
        (async () => {
            setCsv(await getByName(name))
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

    const viewCard = (card: Card) => (
        <div>
            <button onClick={ onNotRemembered }></button>
            <View card={ card } key={ card.cardid }></View>
            <button onClick={ onRemembered }></button>
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
