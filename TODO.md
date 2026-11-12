# TODO

## Обновление платформы

- [x] Обновить платформу сайта: Gatsby 3 → Gatsby 5.16.1, Node.js 16 → Node.js 22 LTS, React 18, Gatsby-плагины и `graphql` 16.
- [x] Привести сборку к воспроизводимому виду: `src/Dockerfile` на `node:22`, CI на Node 22 и Yarn 1, `packageManager` в `package.json`, обновлённый `yarn.lock`. CI использует `yarn install --frozen-lockfile`, поэтому `package-lock.json` и `npm ci` больше не нужны.
- [x] Убрать несовместимый `gatsby-plugin-material-ui` (он для MUI v5, а сайт на MUI v4) и добавить SSR JSS-стилей в `gatsby-ssr.js`.
- [x] Исправить падение статической генерации Gatsby 5. Причина была не в `rehype-react`: Gatsby 5 по умолчанию ставит `trailingSlash: "always"`, поэтому `$path` в page query приходил с завершающим слэшем и `markdownRemark(frontmatter: { path: { eq: $path } })` возвращал `null`. Теперь `gatsby-node.js` передаёт исходный путь в контексте (`mdPath`), а шаблоны сравнивают с ним. Заодно исправлен `gatsby-plugin-sitemap` v6 (`excludes`, `allSitePage.nodes`, `output: "/"` вместо файла `/sitemap.xml`): теперь генерируются `sitemap-index.xml` и `sitemap-0.xml`.
- [x] Проверить после исправления: чистая сборка (`rm -rf .cache public`) `GATSBY_CPU_COUNT=4 NODE_OPTIONS=--max-old-space-size=8192 yarn build` проходит за ~2,5 мин (пик ~2,4 ГБ RSS), 107 HTML-страниц, `yarn prettier-check` без замечаний. На 24 ядрах с настройками по умолчанию сборка нестабильна (воркер падает) — ограничивайте параллелизм `GATSBY_CPU_COUNT`. На VPS с ~2 ГБ RAM нужен swap и `GATSBY_CPU_COUNT=1`.
- [ ] Проверить `gatsby develop` и кастомные плагины из `plugins/` после обновления.

## Удаление старого баннера

- [x] Убрать старый баннер и оставить один источник изображения. Оставлен `src/banner.svg`; `src/images/banner.svg` удалён, `src/components/Footer.js` переключён на общий баннер.
- [x] Старый баннер определён по дате и версии Inkscape: `src/images/banner.svg` был шаблонным Python-баннером, `src/banner.svg` — актуальным Logtalk-баннером.
- [ ] После исправления сборки проверить главную страницу (`data/index.md`, `banner: true`), футер и отсутствие старых хэшированных файлов в `public/static/`.

## Курс

- [ ] Продвинутый уровень: переписать части 11–20 и создать/дописать наборы упражнений (Set11, Set13–Set18; Set16 отсутствует).
- [ ] Привести в порядок нумерацию наборов: `Set4` сейчас пустой, часть 4 использует `Set7`; решить, переименовывать или удалять `Set4`.
- [ ] Расширить покрытие `Set7` тестами для всех упражнений части 4.
