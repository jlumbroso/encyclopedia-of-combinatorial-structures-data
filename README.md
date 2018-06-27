
# Encyclopedia of Combinatorial Structures Dataset

## Introduction

This repository contains a subset of the `algolib` v17 distribution dedicated to the _Encyclopedia of Combinatorial Structures_ (and the `gdev` package by [Bruno Salvy](http://perso.ens-lyon.fr/bruno.salvy/) as an embedded dependency). This subset is used to generate a portable JSON file storing all the information in a convenient format.

The file `ecs.json` contains the entire dictionary's dataset in JSON format:

```json
{
  "1": {
    "id": 1,
    "name": "Alcohols or Unlabelled Non Plane Ternary Trees",
    "description": "Alcohols or unlabelled  non plane ternary Trees",
    "specification": "{S = Union(Z,Prod(Z,Set(S,card = 3))), Z = Atom}",
    "labeled": false,
    "symbol": "S",
    "terms": [
      0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 2,
      0, 0, 4, 0, 0, 8, 0, 0, 17, 0
    ],
    "references": [
      "EIS A000598"
    ]
  },
  ...
```
The specification is in `combstruct` format (see the [combstruct2json](https://github.com/jlumbroso/combstruct2json) project for a parser library and tool to read this format).

## Citation

If this dataset helped your work and you wish to reference it, the usual citation is something like this:

> _The Encyclopedia of Combinatorial Structures_, published electronically at https://ecs.inria.fr, [date].

## History

The Encyclopedia of Combinatorial Structures is a project started by Stéphanie Petit-Halajda in 1998, as she was visiting the Algorithms Project at INRIA in Rocquencourt. It built on the work of many other visitors to the Algorithms Project, and sought to combine the automatic enumeration and construction conveniences of `combstruct` (Marni Mishna, Eithne Murray, Paul Zimmerman, ...) and the automatic asymptotic extraction functionality of `gdev` (Bruno Salvy, Frédéric Chyzak, ...) in a convenient self-contained form. 

![Screenshot of the Encyclopedia of Combinatorial Structures website, as recorded by the Internet Archive, shortly after it was launched in 1998 by Inria's Algorithms Project, under the curation of Stéphanie Petit-Halajda.](/screenshot-ecs-1999.png?raw=true "Screenshot of the ECS website shortly after its launch in 1998, as recorded by the Internet Archive.")

In 2011, Frédéric Chyzak and Alexis Darrasse created a web platform, [DynaMoW](http://ddmf.msr-inria.inria.fr/DynaMoW/), in OCaml, that had strong integration with Maple on the backend. The goal was to enable to creation of websites facilitating interactive exploration. Several specific websites were intended: The [Dynamic Dictionary of Mathematical Functions (DDMF)](http://ddmf.msr-inria.inria.fr/1.9.1/ddmf) and a revamped version of the [Encyclopedia of Combinatorial Structures (ECS)](http://ecs.inria.fr/).

![Screenshot of the Encyclopedia of Combinatorial Structures website, after its port to the dynamic platform DynaMoW, done by Frédéric Chyzak and Alexis Darrasse in 2011, later completed by work by Frédéric Chyzak and Maxence Guesdon.](/screenshot-ecs-2011.png?raw=true "Screenshot of the ECS website after its port to the dynamic platform DynaMoW.")

## References

See [Algolib: The Algorithms Project's Library and Other Packages of the Algorithms Project](http://algo.inria.fr/libraries/software.html), for the original source code release of `algolib` v17, on which this repository is based.