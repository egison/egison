# Documentation for the Egison Programming Language

<https://egison.readthedocs.io/en/latest/>

## Dependency
* [Sphinx](https://www.sphinx-doc.org/en/stable/)
* [Sphinx ReadTheDocs theme](https://sphinx-rtd-theme.readthedocs.io/en/stable/)

## How to build locally
```
$ pip install sphinx sphinx_rtd_theme
$ make html
```

## How to add/manage Japanese translation
NOTE: We have deprecated Japanese version of the documentation.

See <https://docs.readthedocs.io/en/stable/guides/manage-translations.html> for the detail.

### Update `locale/`
```
$ sphinx-build -b gettext . _build/gettext
$ sphinx-intl update -p _build/gettext -l ja
```
