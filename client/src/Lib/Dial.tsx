import React from 'react'

type PropsType = {
    onNext: () => void
    onPrev: () => void
    interval?: number
}

export const Dial: React.FC<PropsType> = props => {
    const interval = props.interval || 200

    const [lock, setLock] = React.useState(false)

    const onWheel = (event: React.WheelEvent) => {
        if (lock) return

        setLock(true)
        const delta = event.deltaY
        if (delta > 0) {
            props.onNext()
        } else if (delta < 0) {
            props.onPrev()
        }
        setTimeout(() => setLock(false), interval)
    }

    return (
        <div onWheel={ onWheel }>
            { props.children }
        </div>
    )
}
