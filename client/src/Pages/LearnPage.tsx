import React, { ReactElement } from 'react'
import { useParams } from 'react-router-dom'

import { BundleID, Bundle, Card } from 'Types'
import { load } from 'Api/Bundle'
import { View } from 'Card/View'
import { Center } from 'Lib/Align'
import { Loading } from 'Lib/Loading'

type PropsType = {
    setMenu: (menu: ReactElement) => void
}
type ParamsType = {
    bundleId: BundleID
}

export const LearnPage : React.FC<PropsType> = props => {
    const { bundleId } = useParams<ParamsType>()

    const [bundle, setBundle] = React.useState<Bundle>(undefined)
    const size = bundle?.cards.length
    const [index, setIndex] = React.useState(0)
    const card = bundle?.cards[index]

    const menu = React.useMemo(() => (
        <>
        </>
    ), [])

    React.useEffect(() => {
        (async () => {
            setBundle(await load(bundleId))
            setIndex(0)
        })()
    }, [bundleId])
    React.useEffect(() => {
        props.setMenu(menu)
    }, [menu])

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

    return (
        <>
            {!!bundle ? (
                <>
                    <Center>
                        <h1>{ bundle.name }</h1>
                    </Center>
                    { viewCard(card) }
                </>
            ): <Loading></Loading>
            }
        </>
    )
}
