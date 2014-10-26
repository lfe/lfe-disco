# ldisco

<img src="resources/images/ldisco.png" />


## Introduction

An LFE client library for the [Disco](https://github.com/discoproject) big-data platform.


## Dependencies

This project assumes that you have [rebar](https://github.com/rebar/rebar)
and [lfetool](https://github.com/lfe/lfetool) installed somwhere
in your ``$PATH``.


## Installation

To inlcude ``ldisco`` in your project, simply update the deps section
of your ``rebar.config``:

.. code:: erlang

    {deps, [
      {ldisco, ".*", {git, "git://github.com/billosys/ldisco.git"}}
    ]}


## Usage

TBD (project still in-progress; worker protocol will likely be the first bit
done)
