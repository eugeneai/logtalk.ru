/**
 * Implement Gatsby's Node APIs in this file.
 *
 * See: https://www.gatsbyjs.org/docs/node-apis/
 */

const path = require("path")

exports.createPages = ({ actions, graphql }) => {
  const { createPage, createRedirect } = actions

  const redirects = [
    ["/part-12/1-functions-as-arguments", "/part-12/1-threads"],
    ["/part-12/2-generators", "/part-12/2-synchronization"],
    ["/part-12/3-functional-programming", "/part-12/3-synchronized"],
    ["/part-12/4-regular-expressions", "/part-12/4-patterns"],
    ["/part-13/1-pygame", "/part-13/1-catch-throw"],
    ["/part-13/2-animation", "/part-13/2-error-terms"],
    ["/part-13/3-events", "/part-13/3-cleanup"],
    ["/part-13/4-more-pygame-techniques", "/part-13/4-custom-errors"],
    ["/part-14/1-game-project", "/part-14/1-dynamic-objects"],
    ["/part-14/2-robot-and-boxes", "/part-14/2-protocols-categories"],
    ["/part-14/3-finishing-the-game", "/part-14/3-delegation"],
    ["/part-14/4-your-own-game", "/part-14/4-reflection"],
    ["/part-17/1-topic", "/part-17/1-rdf"],
    ["/part-18/1-topic", "/part-18/1-planning"],
    ["/part-19/1-topic", "/part-19/1-request-response"],
    ["/part-20/1-topic", "/part-20/1-models"],
  ]

  redirects.forEach(([fromPath, toPath]) => {
    createRedirect({
      fromPath,
      toPath,
      isPermanent: true,
      redirectInBrowser: true,
    })
  })

  const courseMaterialTemplate = path.resolve(
    `src/templates/CourseContentTemplate.js`,
  )

  const coursePartOverviewTemplate = path.resolve(
    `src/templates/CoursePartOverviewTemplate.js`,
  )

  const infoPageTemplate = path.resolve(`src/templates/InfoPageTemplate.js`)

  const vocabularyTemplate = path.resolve(`src/templates/VocabularyTemplate.js`)

  const courseInfoTemplate = path.resolve(`src/templates/CourseInfoTemplate.js`)

  const query = `
  {
    allMarkdownRemark(
      sort: { order: DESC, fields: [frontmatter___path] }
      limit: 1000${
        process.env.NODE_ENV === "production"
          ? `, filter: { frontmatter: { hidden: { ne: true } } }`
          : ""
      }
    ) {
      edges {
        node {
          frontmatter {
            path
            overview
            information_page
            separator_after
            upcoming
            hide_in_sidebar
            sidebar_priority
            vocabulary_page
            course_info_page
          }
        }
      }
    }
  }
    `

  return graphql(query).then((result) => {
    if (result.errors) {
      return Promise.reject(result.errors)
    }

    if (!result.data.allMarkdownRemark) {
      console.log("No markdown pages generated. Did you hide all of them?")
      return
    }

    result.data.allMarkdownRemark.edges.forEach(({ node }) => {
      let template = courseMaterialTemplate
      if (node.frontmatter.overview) {
        template = coursePartOverviewTemplate
      }
      if (node.frontmatter.information_page) {
        template = infoPageTemplate
      }
      if (node.frontmatter.vocabulary_page) {
        template = vocabularyTemplate
      }
      if (node.frontmatter.course_info_page) {
        template = courseInfoTemplate
      }
      if (!node.frontmatter.path) {
        // To prevent a bug that happens in development from time to time
        return
      }
      createPage({
        path: node.frontmatter.path,
        component: template,
        // Gatsby 5 normalizes the page path with a trailing slash, so the
        // page path can no longer be compared with markdown `path` frontmatter
        // directly. Pass the original value explicitly for page queries.
        context: { mdPath: node.frontmatter.path },
      })
    })
  })
}
