import React, { ReactElement } from 'react'
import { useParams } from 'react-router-dom'

import { CardID, Card } from 'Types'
import { View } from 'Card/View'
import { load } from 'Api/Card'

type PropsType = {
    setMenu: (menu: ReactElement) => void
}
type ParamsType = {
    cardId: CardID
}

export const CardPage: React.FC<PropsType> = props => {
    const { cardId } = useParams<ParamsType>()
    const [card, setCard] = React.useState<Card>(undefined)

    const menu = React.useMemo(() => (
        <>
            <button>Edit</button>
            <button>Delete</button>
        </>
    ), [])

    const updateCard = async () => {
        setCard(await load(cardId))
    }

    React.useEffect(() => {
        updateCard()
    }, [cardId])
    React.useEffect(() => {
        props.setMenu(menu)
    }, [menu])

    const loading = (
        <div>
            <span>loading ...</span>
        </div>
    )

    return (
        !!card ? (
            <View card={ card }></View>
        ) : loading
    )
}
