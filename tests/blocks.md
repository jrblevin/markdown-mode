# Block and Paragraph Movement Tests

## With Whitespace

1. abc

    * def

        1. ghi

        1. jkl

mno

```
pqr

st
```

uv

~~~
wx
~~~

  yz
abc

> def
> ghi
>
> jkl
> mno

pqr
stu
vwxyz

## Without Whitespace

1. abc
    * def
        1. ghi
        1. jkl

mno
```
pqr

st
```
uv
~~~
wx
~~~
  yz
abc
> def
> ghi
>
> jkl
> mno
pqr
stu
vwxyz
