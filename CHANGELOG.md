# Changelog

## v3.1.0

* Move `Pool` related stuff into dedicated submodule. Rename `newPool` to `Pool.new`. (@paluh)

## v3.0.0

* Encode PG error and expose them as a result value. (@akheron)

## Expose connection pieces (29/02/2020)
* Expose `connect` and `ConnectResult` (@srghma)

* Remove unused type alias `Database` (@srghma)
---

## v2.1.0 (08/06/2017)
- Add `Foreign` instances.
---

## v2.0.0 (03/06/2017)
- Remove support for tuples as rows, because they are slow and the error messages are bad.
- Add types for rows with up to 19 fields.
- Test `withTransaction`.
- Use `makeAff` and `liftEff` in favor of importing PureScript modules from FFI modules.
---

## v1.0.0 (06/05/2017)
- Clarify that purspgpp is optional.
- Add tests.
---

## v0.0.27 (04/05/2017)
- Update for PureScript 0.11.
