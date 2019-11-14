// Generated from Main.daml
import * as daml from '../../ledger/types';
import { object } from '@mojotech/json-type-validation';

export type Foo<a9J, a9K> = {
  x: Bar<a9J>;
  y: Foo<a9K, a9J>;
  z: a9K;
};
export const Foo = <a9J, a9K>(a9J: daml.Serializable<a9J>, a9K: daml.Serializable<a9K>): daml.Serializable<Foo<a9J, a9K>> => ({
  decoder: () => object({
    x: Bar(a9J).decoder(),
    y: Foo(a9K, a9J).decoder(),
    z: a9K.decoder(),
  }),
});

export type Bar<a9L> = {
  u: a9L;
};
export const Bar = <a9L>(a9L: daml.Serializable<a9L>): daml.Serializable<Bar<a9L>> => ({
  decoder: () => object({
    u: a9L.decoder(),
  }),
});

export type Pair<a9M, a9N> = {
  one: a9M;
  two: a9N;
};
export const Pair = <a9M, a9N>(a9M: daml.Serializable<a9M>, a9N: daml.Serializable<a9N>): daml.Serializable<Pair<a9M, a9N>> => ({
  decoder: () => object({
    one: a9M.decoder(),
    two: a9N.decoder(),
  }),
});

export type Person = {
  name: string;
  party: daml.Party;
  age: daml.Int;
};
export const Person: daml.Serializable<Person> = ({
  decoder: () => object({
    name: daml.Text.decoder(),
    party: daml.Party.decoder(),
    age: daml.Int.decoder(),
  }),
});

export type AllTypes = {
  unit: {};
  bool: boolean;
  int: daml.Int;
  text: string;
  date: daml.Date;
  time: daml.Time;
  party: daml.Party;
  contractId: daml.ContractId<Person>;
  optional: (daml.Int | null);
  list: boolean[];
  textMap: { [key: string]: daml.Party };
  monoRecord: Person;
  polyRecord: Pair<daml.Date, daml.Time>;
};
export const AllTypes: daml.Serializable<AllTypes> = ({
  decoder: () => object({
    unit: daml.Unit.decoder(),
    bool: daml.Bool.decoder(),
    int: daml.Int.decoder(),
    text: daml.Text.decoder(),
    date: daml.Date.decoder(),
    time: daml.Time.decoder(),
    party: daml.Party.decoder(),
    contractId: daml.ContractId(Person).decoder(),
    optional: daml.Optional(daml.Int).decoder(),
    list: daml.List(daml.Bool).decoder(),
    textMap: daml.TextMap(daml.Party).decoder(),
    monoRecord: Person.decoder(),
    polyRecord: Pair(daml.Date, daml.Time).decoder(),
  }),
});
