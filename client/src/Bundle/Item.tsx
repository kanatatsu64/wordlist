import React from 'react'

import style from './ItemStyle.scss'
import { BundleInfo } from 'Types'
import { Delete } from 'Lib/Icon'

type PropsType = {
    bundleInfo: BundleInfo,
    onSelect: (bundleInfo: BundleInfo) => void,
    onDelete: (bundleInfo: BundleInfo) => void
}

export const Item: React.FC<PropsType> = props => {
    const { bundleInfo } = props

    const onDelete = () => props.onDelete(bundleInfo)
    const onSelect = () => props.onSelect(bundleInfo)

    return (
        <div className={ style.item } onClick={ onSelect }>
            <div className={ style.name }>
                <span>{ bundleInfo.name }</span>
            </div>
            <div className={ style.control }>
                <Delete onClick={ onDelete }></Delete>
            </div>
        </div>
    )
}
