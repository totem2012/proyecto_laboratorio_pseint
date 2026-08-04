# Cheatsheet Git — Proyecto PSeInt (equipo de 4)

## Reglas del equipo
- `main` = siempre código que funciona.
- Para algo nuevo: rama con formato `nombre-modulo` (ej: `juan-facturacion`).
- Todo cambio entra a `main` por Pull Request, no por push directo.
- Actualizar tu rama con `main` **seguido**, no dejarlo para el final.

---

## 1. Empezar a trabajar

```bash
git clone <url-del-repo>
cd <repo>
git checkout main
git pull origin main
```

## 2. Crear tu rama

```bash
git checkout -b tunombre-modulo
```

## 3. Guardar tu trabajo

```bash
git add .
git commit -m "mensaje claro de lo que hiciste"
git push origin tunombre-modulo
```

## 4. Traer los cambios de `main` mientras trabajás

Hacerlo cada vez que empezás a trabajar, no solo al final.

```bash
git fetch origin
git merge origin/main
```

- `fetch` = descarga los cambios del remoto, no toca tu código.
- `merge` = mezcla esos cambios con tu rama.

### Si hay conflicto

Git va a marcar el archivo `.psc` así:

```
<<<<<<< HEAD
[tu código]
=======
[código que vino de main]
>>>>>>> origin/main
```

1. Editar a mano, dejar el código correcto.
2. Borrar las marcas `<<<<<<<`, `=======`, `>>>>>>>`.
3. **Abrir el archivo en PSeInt y correrlo** para chequear que no quedó nada roto (llaves, `FinPara`, `FinSegun`, etc).
4. Guardar y confirmar el merge:

```bash
git add algoritmo.psc
git commit
git push origin tunombre-modulo
```

## 5. Pedir el Pull Request

Cuando tu módulo funciona y ya está actualizado con `main`:
1. Push de tu rama (si no lo hiciste ya).
2. Ir a GitHub → Pull Request → base: `main` ← compare: `tunombre-modulo`.
3. Que alguien del equipo lo revise antes de aprobar.

---

## Comandos de consulta útiles

| Comando | Qué hace |
|---|---|
| `git status` | Ver qué cambió y en qué rama estás |
| `git branch` | Ver todas las ramas locales |
| `git log --oneline --graph --all` | Ver historial de todas las ramas |
| `git diff main origin/main` | Ver qué trajo `main` antes de mergear |

---

## Tips para evitar conflictos en el `.psc`

- Delimitar cada módulo con comentarios:
  ```
  // ===== INICIO MODULO FACTURACION =====
  ...
  // ===== FIN MODULO FACTURACION =====
  ```
- Cada uno trabaja sobre su propia subrutina/función, evitar tocar código ajeno.
- PRs chicos y seguidos, no guardarse todo para el final.
- Antes de cualquier commit después de un merge: **correr el algoritmo en PSeInt**.
