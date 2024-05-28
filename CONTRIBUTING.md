<!--
SPDX-FileCopyrightText: 2024 Cognitive Disorders Research Lab

SPDX-License-Identifier: Apache-2.0
SPDX-License-Identifier: MIT
-->
<!-- vale Google.FirstPerson = NO -->
<!-- omit in toc -->
# Contributing to Kaleidoscope

<!-- vale Google.Exclamation = NO -->
Thank you for taking your time to contribute to this project! Your efforts are greatly appreciated. ❤️
<!-- vale Google.Exclamation = YES -->

We welcome and encourage all types of contributions. Please see the [Table of Contents](#table-of-contents) for different ways to help and details about how this project handles them. Please make sure to read the relevant section before making your contribution. Following the contribution guidelines makes it a lot easier for the maintainers and smooth out the experience for all involved. The community looks forward to your contributions.

If you like the project, but just don't have time to contribute, that's fine. You can still support the project and show your appreciation in other ways, which we also appreciate:

- Star the project
- Tweet about it
- Cite the project in your publications if you found it helpful
- Refer this project in your project's README
- Mention the project at local meetups and tell your friends/colleagues

<!-- omit in toc -->
## Table of Contents

- [I Have a Question](#i-have-a-question)
- [I Want To Contribute](#i-want-to-contribute)
  - [Reporting Bugs](#reporting-bugs)
  - [Suggesting Enhancements](#suggesting-enhancements)
  - [Your First Code Contribution](#your-first-code-contribution)
  - [Improving The Documentation](#improving-the-documentation)
- [Style Guides](#style-guides)
  - [Commit Messages](#commit-messages)
- [Join The Project Team](#join-the-project-team)



## I Have a Question

Before you ask a question, it's best to search for existing [Issues](https://github.com/CogDisResLab/kaleidoscope/issues) that might help you. In case you have found a suitable issue and still need clarification, you can write your question in this issue. It's also advisable to search the internet for answers first.

If you then still feel the need to ask a question and need clarification, we recommend the following:

- Open an [Issue](https://github.com/CogDisResLab/kaleidoscope/issues/new).
- Provide as much context as you can about what you're running into.
- Provide project and platform versions (`rustc -V`, etc), depending on what seems relevant.

We then take care of the issue as soon as possible.

## I Want To Contribute

### Legal Notice <!-- omit in toc -->

When contributing to this project, you must agree that you have authored 100% of the content, that you have the necessary rights to the content and that the content you contribute may be provided under the project license.


This project is dual-licensed under [MIT](LICENSE-MIT) and [Apache 2.0](LICENSE-APACHE). When you submit changes, your submissions are understood to be under the same [MIT](LICENSE-MIT) and [Apache 2.0](LICENSE-APACHE) that covers the project. Feel free to contact the maintainers if that's a concern.


### Reporting Bugs

<!-- omit in toc -->
#### Before Submitting a Bug Report

A good bug report shouldn't leave others needing to chase you up for more information. Therefore, we ask you to investigate carefully, collect information and describe the issue in detail in your report. Please complete the following steps in advance to help us fix any potential bug as fast as possible.

- Make sure that you are using the latest version.
- Determine if your bug is really a bug and not an error on your side e.g. using incompatible environment components/versions (If you are looking for support, you might want to check [this section](#i-have-a-question)).
- To see if other users have experienced (and potentially already solved) the same issue you are having, check if there is not already a bug report existing for your bug or error in the [bug tracker](https://github.com/CogDisResLab/kaleidoscope/issues?q=label%3Abug).
- Also make sure to search the internet (including Stack Overflow) to see if users outside of the GitHub community have discussed the issue.
- Collect information about the bug:
  - Stack trace (Traceback). We use RUST_BACKTRACE=1 to get a full stack trace.
  - OS, Platform and Version (Windows, Linux, macOS, x86, ARM)
  - Version of Rust, Cargo, and other environment components if applicable
  - Possibly your input and the output
  - Can you reliably reproduce the issue? And can you also reproduce it with older versions?

<!-- omit in toc -->
#### How Do I Submit a Good Bug Report?


We use GitHub issues to track bugs and errors. If you run into an issue with the project:

- Open an [Issue](https://github.com/CogDisResLab/kaleidoscope/issues/new). (Since we can't be sure at this point whether it is a bug or not, we ask you not to talk about a bug yet and not to label the issue.)
- Explain the behavior you would expect and the actual behavior.
- Please provide as much context as possible and describe the *reproduction steps* that someone else can follow to recreate the issue on their own. This usually includes your code. For good bug reports you should isolate the problem and create a reduced test case.
- Provide the information you collected in the previous section.

Once it's filed:

- The project team will label the issue accordingly.
- A team member will try to reproduce the issue with your provided steps. If there are no reproduction steps or no obvious way to reproduce the issue, the team will ask you for those steps and mark the issue as `needs-repro`. Bugs with the `needs-repro` tag will not be addressed until they are reproduced.
- If the team is able to reproduce the issue, it will be marked `needs-fix`, as well as possibly other tags (such as `critical`), and the issue will be left to be [implemented by someone](#your-first-code-contribution).


### Suggesting Enhancements

This section guides you through submitting an enhancement suggestion for Gainful Key, **including completely new features and minor improvements to existing functionality**. Following these guidelines will help maintainers and the community to understand your suggestion and find related suggestions.

<!-- omit in toc -->
#### Before Submitting an Enhancement

- Make sure that you are using the latest version.
- Perform a [search](https://github.com/CogDisResLab/kaleidoscope/issues) to see if the enhancement has already been suggested. If it has, add a comment to the existing issue instead of opening a new one.
- Find out whether your idea fits with the scope and aims of the project. It's up to you to make a strong case to convince the project's developers of the merits of this feature. Keep in mind that we want features that will be useful to the majority of our users and not just a small subset. If you're just targeting a minority of users, consider writing an add-on/plugin library.

<!-- omit in toc -->
#### How Do I Submit a Good Enhancement Suggestion?

Enhancement suggestions are tracked as [GitHub issues](https://github.com/CogDisResLab/kaleidoscope/issues).

- Use a **clear and descriptive title** for the issue to identify the suggestion.
- Provide a **step-by-step description of the suggested enhancement** in as many details as possible.
- **Describe the current behavior** and **explain which behavior you expected to see instead** and why. At this point you can also tell which alternatives do not work for you.
- You may want to **include screenshots and animated GIFs** which help demonstrate the steps or point out the part which the suggestion is related to. You can use [this tool](https://www.cockos.com/licecap/) to record GIFs on macos and Windows, and [this tool](https://github.com/colinkeenan/silentcast) or [this tool](https://github.com/GNOME/byzanz) on Linux. <!-- this should only be included if the project has a GUI -->
- **Explain why this enhancement would be useful** to most `aaprop` users. You may also want to point out the other projects that solved it better and which could serve as inspiration.

### Your First Code Contribution
<!-- TODO
include Setup of env, IDE and typical getting started instructions?

-->

### Improving The Documentation
<!-- TODO
Updating, improving and correcting the documentation

-->

## Style Guides

We use `rustfmt`, `clippy` and `cargo check` to ensure a consistent code style. We also use `cargo test` to ensure that all tests pass. Please do not allow specific `clippy` lints without discussion with the team first.

### Commit Messages

The project adheres to the Conventional Commits specification for commit messages, which enhances readability and ease of understanding when navigating through the project history and serves as the basis for generating the Gainful Key change log.

## Join The Project Team

Please [open an issue](https://github.com/CogDisResLab/kaleidoscope/issues) to let the team know that of your interest in joining, and we can discuss the details in the issue.

<!-- omit in toc -->
## Attribution

<!-- vale write-good.Passive = NO -->
<!-- vale Google.Passive = NO -->
This guide is based on the **contributing-gen**. [Make your own](https://github.com/bttger/contributing-gen)
<!-- vale Google.Passive = YES -->
<!-- vale write-good.Passive = YES -->
