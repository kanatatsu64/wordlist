import React from 'react'
import { useHistory, useParams } from 'react-router-dom'

import { Bundle, Card } from 'Types'
import { getByName } from 'Api/Bundle'
import { Center } from 'Lib/Align'

type PropsType = { }

export const BundleTablePage: React.FC<PropsType> = props => {
    const { name } = useParams()

    const history = useHistory()
    const [bundle, setBundle] = React.useState<Bundle>(undefined)
    const cards = bundle?.cards

    React.useEffect(() => {
        (async () => {
            setBundle(await getByName(name))
        })()
    }, [name])

    const onStartLearning = () => {
        history.push(`/bundle/learn/${ name }`)
    }

    const viewCards = (cards: Card[]) => (
        <table>
            <tbody>
            { cards.map(viewCard) }
            </tbody>
        </table>
    )

    const viewCard = (card: Card) => {
        const { word, meaning, cardid } = card
        return (
            <tr key={ cardid }>
                <td><Center>{ word }</Center></td>
                <td><Center>{ meaning }</Center></td>
            </tr>
        )
    }

    const loading = (
        <div>
            <span>loading ...</span>
        </div>
    )

    return (
        <>
            <Center>
                <h1>{ name }</h1>
                <button onClick={ onStartLearning }>Start Learning</button>
            </Center>
            { !!cards ? viewCards(cards) : loading }
        </>
    )
}
