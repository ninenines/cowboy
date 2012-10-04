Contributing
============

Introduction
------------

This document describes the usages and rules to follow when contributing
to this project.

It uses the uppercase keywords SHOULD for optional but highly recommended
conditions and MUST for required conditions.

`git` is a distributed source code versioning system. This document refers
to three different repositories hosting the source code of the project.
`Your local copy` refers to the copy of the repository that you have on
your computer. The remote repository `origin` refers to your fork of the
project's repository that you can find in your GitHub account. The remote
repository `upstream` refers to the official repository for this project.

Following this document will ensure prompt merging of your work in the
`master` branch of the project.

Planning
--------

Upon identifying a bug, you SHOULD submit a ticket, regardless of your
plans for fixing it. If you plan to fix the bug, you SHOULD discuss your
plans to avoid having your work rejected.

Before implementing a new feature, you SHOULD submit a ticket for discussion
on your plans. The feature might have been rejected already, or the
implementation might already be decided.

Cloning
-------

You MUST fork the project's repository to your GitHub account by clicking
on the `Fork` button.

Then, from your fork's page, copy the `Git Read-Only` URL to your clipboard.
You MUST perform the following commands in the folder you choose, replacing
`$URL` by the URL you just copied, `$UPSTREAM_URL` by the `Git Read-Only`
project of the official repository, and `$PROJECT` by the name of this project.

``` bash
$ git clone "$URL"
$ cd $PROJECT
$ git remote add upstream $UPSTREAM_URL
```

Branching
---------

Before starting working on the code, you MUST update to `upstream`. The
project is always evolving, and as such you SHOULD always strive to keep
up to date when submitting patches to make sure they can be merged without
conflicts.

To update the current branch to `upstream`, you can use the following commands.

``` bash
$ git fetch upstream
$ git rebase upstream/master
```

It may ask you to stash your changes, in which case you stash with:

``` bash
$ git stash
```

And put your changes back in with:

``` bash
$ git stash pop
```

You SHOULD use these commands both before working on your patch and before
submitting the pull request. If conflicts arise it is your responsability
to deal with them.

You MUST create a new branch for your work. First, ensure you are on `master`.
You MUST update `master` to `upstream` before doing anything. Then create a
new branch `$BRANCH` and switch to it.

``` bash
$ git checkout -b $BRANCH
```

You MUST use a an insightful branch name.

If you later need to switch back to an existing branch `$BRANCH`, you can use:

``` bash
$ git checkout $BRANCH
```

Source editing
--------------

The following rules MUST be followed:
 *  Indentation uses horizontal tabs, 1 tab equals the length of 4 space chars
 *  Do NOT align code; only indentation is allowed

The following rules SHOULD be followed:
 *  Write small functions whenever possible
 *  Avoid having too many clauses containing clauses containing clauses

Committing
----------

You MUST ensure that all commits pass all tests and do not have extra
Dialyzer warnings.

You MUST put all the related work in a single commit. Fixing a bug is one
commit, adding a feature is one commit, adding two features is two commits.

You MUST write a proper commit title and message. The commit title MUST be
at most 72 characters; it is the first line of the commit text. The second
line of the commit text MUST be left blank. The third line and beyond is the
commit message. You SHOULD write a commit message. If you do, you MUST make
all lines smaller than 80 characters. You SHOULD explain what the commit
does, what references you used and any other information that helps
understanding your work.

Submitting the pull request
---------------------------

You MUST push your branch `$BRANCH` to GitHub, using the following command:

``` bash
$ git push origin $BRANCH
```

You MUST then submit the pull request by using the GitHub interface.
You SHOULD provide an explanatory message and refer to any previous ticket
related to this patch.
