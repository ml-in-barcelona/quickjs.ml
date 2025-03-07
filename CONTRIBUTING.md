This is the contribute.md of our project. Great to have you here. Here are a few ways you can help make this project better!

# Contribute.md

## How to Contribute


### 1. Setup && instalation

We first need to setup the quickjs submodule, by running the following: 

```sh
$ git submodule init

$ git submodule update --recursive --force
```

To setup the OCaml project, install the dependencies and build the project, we can simply run:

```sh
$ make init

$ make dev
```

### 2. Reporting Issues

If you encounter a bug, have a feature request, or notice something that needs improvement, please report it on our [GitHub Issues page](https://github.com/ml-in-barcelona/quickjs.ml/issues). When submitting an issue, try to include:
- A clear description of the problem.
- Steps to reproduce the issue (if applicable).
- Expected behavior vs. actual behavior.
- Any relevant logs or error messages.

### 3. Fixing Bugs and Enhancements

You can browse the [Issues](https://github.com/ml-in-barcelona/quickjs.ml/issues) page for open issues (or create your own). 
If you’d like to work on an issue:
1. Comment on the issue to let others know you’re working on it.
2. Fork the repository and create a new branch for your changes.
3. Make your changes and ensure they follow the coding style (see below).
4. Test your changes before submitting a pull request (PR).
5. Open a PR and provide a clear description of the changes.

### 4. Documentation Contributions

Our documentation is generated using **odoc** and is available [here](https://ml-in-barcelona.github.io/quickjs.ml/docs/local/quickjs/index.html). If you find missing or unclear sections, feel free to improve them by updating the documentation files and submitting a PR.

### 5. Code Style and Guidelines

To keep contributions consistent:
- Follow OCaml best practices.
- Use `ocamlformat` for formatting.
- Keep commits clean and atomic.
- Write clear commit messages.

### 6. Running Tests

Before submitting changes, please test your modifications to ensure nothing is broken. Run:
```sh
make test
```

If you add new functionality, consider adding tests.

### 7. Submitting a Pull Request

Ensure your changes are rebased on the latest main branch.
Provide a clear description of your changes in the PR.
If applicable, update documentation and tests.
Request a review from one of the maintainers.

### 8. Communication

If you have questions or need help, feel free to:

Open a GitHub Discussion.
Comment on an issue or PR.
Reach out via any communication channels listed in the repository.

License
By contributing to this project, you agree that your contributions will be licensed under the same [license](./LICENSE.md) as the project.

Happy coding, and thank you for contributing!

