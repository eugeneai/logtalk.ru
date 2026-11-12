# TODO

## Обновление платформы

- [x] Обновить платформу сайта: Gatsby 3 → Gatsby 5.16.1, Node.js 16 → Node.js 22 LTS, React 18, Gatsby-плагины и `graphql` 16.
- [x] Привести сборку к воспроизводимому виду: `src/Dockerfile` на `node:22`, CI на Node 22 и Yarn 1, `packageManager` в `package.json`, обновлённый `yarn.lock`. CI использует `yarn install --frozen-lockfile`, поэтому `package-lock.json` и `npm ci` больше не нужны.
- [x] Убрать несовместимый `gatsby-plugin-material-ui` (он для MUI v5, а сайт на MUI v4) и добавить SSR JSS-стилей в `gatsby-ssr.js`.
- [ ] Исправить падение статической генерации Gatsby 5: `property-information/lib/aria.js` и `rehype-react` v6 несовместимы с React 18. Нужно обновить `rehype-react` (или заменить связку `rehype-react`/`property-information`) и проверить рендеринг Markdown.
- [ ] Проверить после исправления: `GATSBY_CPU_COUNT=1 NODE_OPTIONS=--max-old-space-size=1024 npm run build`, `npm run prettier-check`, работу `gatsby develop` и кастомных плагинов из `src/plugins/`. На текущей машине (1,9 ГБ RAM) сборка Gatsby 5 доходит до генерации HTML, но падает на несовместимости `rehype-react`; для полной проверки нужна машина с большим объёмом памяти.

## Удаление старого баннера

- [x] Убрать старый баннер и оставить один источник изображения. Оставлен `src/banner.svg`; `src/images/banner.svg` удалён, `src/components/Footer.js` переключён на общий баннер.
- [x] Старый баннер определён по дате и версии Inkscape: `src/images/banner.svg` был шаблонным Python-баннером, `src/banner.svg` — актуальным Logtalk-баннером.
- [ ] После исправления сборки проверить главную страницу (`data/index.md`, `banner: true`), футер и отсутствие старых хэшированных файлов в `public/static/`.

## Курс

- [ ] Продвинутый уровень: переписать части 11–20 и создать/дописать наборы упражнений (Set11, Set13–Set18; Set16 отсутствует).
- [ ] Привести в порядок нумерацию наборов: `Set4` сейчас пустой, часть 4 использует `Set7`; решить, переименовывать или удалять `Set4`.
- [ ] Расширить покрытие `Set7` тестами для всех упражнений части 4.
