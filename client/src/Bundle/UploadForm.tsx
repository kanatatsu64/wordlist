import React from 'react'

import { BundleID, PluginInfo, PluginID } from 'Types'
import { pluginIds } from 'Plugin'
import { loadInfo } from 'Api/Plugin'
import style from './UploadFormStyle.scss'
import { upload } from 'Api/Bundle'

type PropsType = {
    bundleId: BundleID
    onUpload?: () => void
    onUploaded?: () => void
}

export const UploadForm: React.FC<PropsType> = props => {
    const { bundleId } = props

    const [plugins, setPlugins] = React.useState<PluginInfo[]>([])
    const [pluginId, setPluginId] = React.useState<PluginID>(undefined)
    const [csv, setCsv] = React.useState<File>(undefined)

    React.useEffect(() => {
        (async () => {
            const plugins: PluginInfo[] = []
            await Promise.all(pluginIds.map(async pluginId => {
                plugins.push(await loadInfo(pluginId))
            }))
            setPlugins(plugins)
            if (plugins.length > 0) setPluginId(plugins[0].pluginid)
            else setPluginId(undefined)
        })()
    }, [])

    const onPluginChange = (event: React.ChangeEvent<HTMLSelectElement>) => {
        setPluginId(event.target.value)
    }
    const onCsvChange = (event: React.ChangeEvent<HTMLInputElement>) => {
        const files = event.target.files
        if (files.length == 1) setCsv(files[0])
        else if (files.length == 0) setCsv(undefined)
        else alert("Please choose exactly one file.")
    }

    const onSubmit = async (event: React.FormEvent<HTMLFormElement>) => {
        event.preventDefault()
        if (!pluginId) {
            alert("Please select plugin.")
            return
        }
        if (!csv) {
            alert("Please choose a CSV file.")
            return
        }
        if (props.onUpload) props.onUpload()
        await upload(bundleId, csv, pluginId)
        if (props.onUploaded) props.onUploaded()
    }

    const options = plugins.map(plugin => (
        <option value={ plugin.pluginid } key={ plugin.pluginid }>{ plugin.name }</option>
    ))

    return (
        <form onSubmit={ onSubmit }>
            <label className={ style.item }>
                plugin
                <select name="plugin" onChange={ onPluginChange } value={ pluginId }>
                    { options }
                </select>
            </label>
            <label className={ style.item }>
                csv
                <input type="file" name="csvFile" onChange={ onCsvChange }></input>
            </label>
            <input type="submit" value="Upload CSV"></input>
        </form>
    )
}
