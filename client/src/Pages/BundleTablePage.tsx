import React, { ReactElement } from 'react'
import { useHistory, useParams } from 'react-router-dom'

import { Bundle, Card } from 'Types'
import { load } from 'Api/Bundle'
import { Center } from 'Lib/Align'
import { Sequence } from 'Lib/Sequence'
import { Dial } from 'Lib/Dial'
import { mod } from 'Utils'

type PropsType = {
    setMenu: (menu: ReactElement) => void
}

export const BundleTablePage: React.FC<PropsType> = props => {
    const { bundleId } = useParams()

    const history = useHistory()
    const [bundle, setBundle] = React.useState<Bundle>(undefined)
    const [from, setFrom] = React.useState(0)
    const cards = bundle?.cards

    React.useEffect(() => {
        (async () => {
            setBundle(await load(bundleId))
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
            {!!bundle ? (
                <>
                    <Center>
                        <h1>{ bundle.name }</h1>
                        <button onClick={ onStartLearning }>Start Learning</button>
                    </Center>
                    { viewCards(cards) }
                </>
            ): loading
            }
        </>
    )
}
