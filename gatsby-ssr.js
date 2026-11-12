/**
 * Server-side rendering of Material-UI v4 (JSS) styles.
 * See https://material-ui.com/guides/server-rendering/
 */
const React = require("react")
const { ServerStyleSheets } = require("@material-ui/styles")

let sheets

exports.wrapRootElement = ({ element }) => {
  sheets = new ServerStyleSheets()
  return sheets.collect(element)
}

exports.onRenderBody = ({ setHeadComponents }) => {
  if (sheets) {
    setHeadComponents([
      <style
        id="jss-server-side"
        key="jss-server-side"
        dangerouslySetInnerHTML={{ __html: sheets.toString() }}
      />,
    ])
  }
}
