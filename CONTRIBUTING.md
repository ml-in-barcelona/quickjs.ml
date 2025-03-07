# How to contribute to quickjs.ml

Great to have you here. Here are a few ways you can help make this project better!

## 1. Reporting Issues

If you encounter a bug, have a feature request, or notice something that needs improvement, please report it on our [issues page](https://github.com/ml-in-barcelona/quickjs.ml/issues). When submitting an issue, try to include:
- A clear description of the problem.
- Steps to reproduce the issue (if applicable).
- Expected behavior vs. actual behavior.
- Any relevant logs or error messages.

## 2. Documentation Contributions

Our documentation is generated using **odoc** and is available [here](https://ml-in-barcelona.github.io/quickjs.ml/docs/local/quickjs/index.html). If you find missing or unclear sections, feel free to improve them by updating the documentation files and submitting a PR.

## 3. Fixing bugs and enhancements

You can browse the [issues page](https://github.com/ml-in-barcelona/quickjs.ml/issues) for open issues or create your own and start working on it.
If you’d like to work on an issue that hasn't been assigned to anyone, please comment on the issue to let others know you’re working on it, after:
1. Fork the repository and creating a new branch for your changes.
2. Making your changes and ensuring they follow the coding style (see below).
3. Testing your changes before submitting a pull request.
4. Opening a PR and providing a clear description of the changes.

# How to develop

## 0. Clone the repository

```sh
$ git clone https://github.com/ml-in-barcelona/quickjs.ml.git
```

## 1. Instalation

We first need to checkout the quickjs submodule, by running the following:

```sh
$ git submodule init
$ git submodule update --recursive --force
```

To setup the OCaml project, install the dependencies and build the project, we can simply run:

```sh
$ make init # creates the opam switch and installs the dependencies (including lsp and ocamlformat)
$ make dev # builds the project in watch mode
```

## 2. Running Tests

Before submitting changes, please test your modifications to ensure nothing is broken. Run:
```sh
$ make test
```
If you add new functionality, adding tests is highly encouraged.

To run an isolated case, you can run the [./demo/demo.ml](./demo/demo.ml) executable with:
```sh
$ make demo
```

## 3. Code Style and Guidelines

To keep contributions consistent:
- Follow OCaml best practices.
- Use `ocamlformat` for formatting.
- Keep commits clean and atomic.
- Write clear commit messages.

## 4. Submitting a Pull Request

Ensure your changes are rebased on the latest main branch.
Provide a clear description of your changes in the PR.
If applicable, update documentation and tests.
Request a review from one of the maintainers.

## 5. Communication

If you have questions or need help, feel free to:

Open a GitHub Discussion.
Comment on an issue or PR.
Reach out via any communication channels listed in the repository.

## License
By contributing to this project, you agree that your contributions will be licensed under the same [license](./LICENSE.md) as the project.

Happy coding, and thank you for contributing!
