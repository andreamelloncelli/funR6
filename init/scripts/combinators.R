# /*
#
# 	Problema: trasformare il seguente CSV
#
# */

good = "foo;foo@google.com
bar;bar@hotmail.com"

# 	/*
#
# 		in una lista di
#
# 	*/
#

contactInfo <-
	list(
		name  = "",
		email = ""
	)

# 	/*
#
# 		Un primo tentativo...
#
# 	*/

# split <- setGeneric("split")
split.character <- function(string, pattern, ...) {
	if (length(string) == 1) {
		out <- str_split(string = string, pattern = pattern, ...) %>% unlist
	} else {
		out <- str_split(string = string, pattern = pattern, ...)
	}
}

library(stringr)
library(purrr)
library(magrittr)


toContactInfos1 <- function(csv) {
	csv %>%
		str_split("\n") %>%
		map(function(x, line) str_split(x, ";") ) %>%
		map(function(x) map(x, function(ls) list(name = ls[1], email = ls[2]))) %>%
		.[[1]]
}

toContactInfos1(good)

# 	/*
# 		[ { name: 'foo', email: 'foo@google.com' },
# 			{ name: 'bar', email: 'bar@hotmail.com' } ]
# 	*/
#
# 		/*
#
# 		Cosa succede se il CSV non ha tutti campi?
#
# 		*/

bad <- "foo;foo@google.com
bar;bar@hotmail.com
;baz@yahoo.com
quux;"

toContactInfos1(bad)

# 	/*
# 		[ { name: 'foo', email: 'foo@google.com' },
# 			{ name: 'bar', email: 'bar@hotmail.com' },
# 			{ name: '', email: 'baz@yahoo.com' },
# 			{ name: 'quux', email: '' } ]
# 	*/
#
# 		/*
#
# 		Potremmo aggiungere la possibilità di specificare delle opzioni
#
# 	*/

toContactInfos2 <- function(csv,
														nameRequired,
														emailRequired)
{
	csv %>%
		str_split("\n") %>%
		map( function(line) str_split(line, ';') ) %>% # in csv in lines in fields
		map( function(x) Filter(x = x, f = function(x) {
			names(x) <- c("name", "email")
			message("aaa", x[["name"]])
			if (
				(x[["name"]] == '' && nameRequired) ||
				(x[["email"]] == '' && emailRequired)
			) {
				return(FALSE)
			}
			return(TRUE)
		}) ) %>%
		map( function(x) map(x, function(x) c( name = x[[1]], email = x[[2]] )) )
}
toContactInfos2(bad, TRUE, TRUE)

# 	/*
# 		[ { name: 'foo', email: 'foo@google.com' },
# 			{ name: 'bar', email: 'bar@hotmail.com' } ]
# 	*/
#
# 		/*
#
# 		La soluzione sembra funzionare ma cosa ne pensate?
#
# 		*/
#
# 		/*
#
# 		Separiamo la logica di parsing da quella di filtering e mapping
#
# 	*/
#
# 		type Pair<A> = [A, A]
#
# 	const toPair = (xs: Array<string>): Pair<string> => [
# 		xs.length > 0 ? xs[0].trim() : '',
# 		xs.length > 1 ? xs[1].trim() : ''
# 		]
#
toPair <- function(xs) {
	if (length(xs) > 0)
		return(xs[[1]])
	else
		return("")
}
toPair(xs)

# 	// parsing
# 	const getTokens = (csv: string): Array<Pair<string>> =>
# 		csv.split('\n').map(line => toPair(line.split(';')))
#
getTokens <- function(csv) {
	csv %>%
		str_split("\n") %>%
		map( function(line) str_split(line, ';') ) %>%
		map( function(x) map(x, function(x) {names(x) <- c("name", "email"); x} ))
}
getTokens(csv)
# 	// mapping
# 	const toContactInfo = ([name, email]: Pair<string>): ContactInfo => ({
# 												 	name,
# 												 	email
# 												 })
toContactInfo <- function(pair) {
	c(name = pair[["name"]],
		email = pair[["email"]])
}
# 	const toContactInfos3 = (
# 		csv: string,
# 		nameRequired: boolean,
# 		emailRequired: boolean
# 	): Array<ContactInfo> =>
# 		// parsing
# 	getTokens(csv)
# 	// filtering
# 	.filter(([name, email]) => {
# 		if (
# 			(name === '' && nameRequired) ||
# 			(email === '' && emailRequired)
# 		) {
# 			return false
# 		}
# 		return true
# 	})
# 	// mapping
# 	.map(toContactInfo)
#
toContactInfos3 <- function(csv, nameRequired, emailRequired) {
	getTokens(csv) %>%
		Filter( function(pair) {
			if (
				(pair[["name"]] == ""  && nameRequired) ||
				(pair[["email"]] == "" && emailRequired)
			) {
				return(FALSE)
			}
			return(TRUE)
		}) %>%
		map(toContactInfo)
}

# 	/*
#
# 		Separiamo anche la logica di filtering
#
# 	*/
#
# 		// Un "predicato" su `A` è una funzione che accetta in input un valore
# 	// di tipo `A` e restituisce un booleano
# 	type Predicate<A> = (a: A) => boolean
#
# 	// Possiamo ora rendere formale il concetto di "filtro" per le righe del CSV
# 	// Definizione: chiamiamo "filtro" un predicato su `Pair<string>`
# 	type Filter = Predicate<Pair<string>>
#
# 		// filtering
# 	const getFilter = (
# 		nameRequired: boolean,
# 		emailRequired: boolean
# 	): Filter => ([name, email]) => {
# 		if (
# 			(name === '' && nameRequired) ||
# 			(email === '' && emailRequired)
# 		) {
# 			return false
# 		}
# 		return true
# 	}
getFilter <- function(
	nameRequired,
	emailRequired
) {
	function(pair) {
		stopifnot(length(pair) == 2 &&
								names(pair) == c("name", "email"))
		if (
			(pair[["name"]] == ""  && nameRequired) ||
			(pair[["email"]] == "" && emailRequired)
		) {
			return(FALSE)
		}
		return(TRUE)
	}
}
#
# 	const toContactInfos4 = (
# 		csv: string,
# 		nameRequired: boolean,
# 		emailRequired: boolean
# 	): Array<ContactInfo> =>
# 		getTokens(csv)
# 	.filter(getFilter(nameRequired, emailRequired))
# 	.map(toContactInfo)
toContactInfos4 <- function(csv, nameRequired, emailRequired) {
	getTokens(csv) %>%
		map(function(x) Filter(x, f = getFilter(nameRequired, emailRequired))) %>%
		map(function(x) map(x, toContactInfo))
}

toContactInfos4(csv, TRUE,TRUE)
#
# 	/*
#
# 		Fino ad ora ho fatto un refactoring che non ha modificato in alcun modo la funzionlità
# 	Ma adesso è evidente che i parametri aggiuntivi servono solo per creare il predicato.
# 	Ma allora passiamo direttamente il predicato come argomento!
#
# 		*/
#
# 		const toContactInfos = (
# 			csv: string,
# 			filter: Filter
# 		): Array<ContactInfo> =>
# 		getTokens(csv)
# 	.filter(filter)
# 	.map(toContactInfo)
#
# 	/*
#
# 		Molto meglio. Ma come costruire un filtro?
# 		Anche getFilter non è del tutto soddisfacente.
# 	Usiamo un combinatore!
#
# 		Un combinatore su un tipo `A` è una funzione `combinator: A -> A`
#
# 	*/
#
# 		type Combinator = (filter: Filter) => Filter
#
# 	const all: Filter = () => true
#
# 	const nameRequired: Combinator = next => pair =>
# 		pair[0] === '' ? false : next(pair)
#
# 	const emailRequired: Combinator = next => pair =>
# 		pair[1] === '' ? false : next(pair)
#
# 	console.log(toContactInfos(bad, all))
# 	/*
# 		[ { name: 'foo', email: 'foo@google.com' },
# 			{ name: 'bar', email: 'bar@hotmail.com' },
# 			{ name: '', email: 'baz@yahoo.com' },
# 			{ name: 'quux', email: '' } ]
# 	*/
#
# 		console.log(toContactInfos(bad, nameRequired(all)))
# 	/*
# 		[ { name: 'foo', email: 'foo@google.com' },
# 			{ name: 'bar', email: 'bar@hotmail.com' },
# 			{ name: 'quux', email: '' } ]
# 	*/
#
# 		console.log(
# 			toContactInfos(bad, emailRequired(nameRequired(all)))
# 		)
# 	/*
# 		[ { name: 'foo', email: 'foo@google.com' },
# 			{ name: 'bar', email: 'bar@hotmail.com' } ]
# 	*/
