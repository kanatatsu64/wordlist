import React from 'react'
import { useHistory, useParams } from 'react-router-dom'

import { Bundle, Card } from 'Types'
import { getByName } from 'Api/Bundle'
import { Center } from 'Lib/Align'
import { Sequence } from 'Lib/Sequence'
import { Dial } from 'Lib/Dial'
import { mod } from 'Utils'

type PropsType = { }

export const BundleTablePage: React.FC<PropsType> = props => {
    const { name } = useParams()

    const history = useHistory()
    const [bundle, setBundle] = React.useState<Bundle>(undefined)
    const [from, setFrom] = React.useState(0)
    const cards = bundle?.cards

    React.useEffect(() => {
        (async () => {
            setBundle(await getByName(name))
        })()
    }, [name])

    const onStartLearning = () => {
        history.push(`/bundle/learn/${ name }`)
    }

    const onNext = () => {
        setFrom(mod(from + 1, cards.length))
    }

    const onPrev = () => {
        setFrom(mod(from - 1, cards.length))
    }

    const viewCards = (cards: Card[]) => (
        <Dial onNext={ onNext } onPrev={ onPrev }>
            <Center>
            <table>
                <tbody>
                    <Sequence from={ from } count={ 3 }>
                        {cards.map(card => {
                            const { word, meaning, cardid } = card
                            return (
                                <tr key={ cardid }>
                                    <td><Center>{ word }</Center></td>
                                    <td><Center>{ meaning }</Center></td>
                                </tr>
                            )
                        })}
                    </Sequence>
                </tbody>
            </table>
            </Center>
        </Dial>
    )

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
            {!!cards ? viewCards(cards) : loading}
        </>
    )
}
