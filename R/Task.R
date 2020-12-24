# Task <- R6::R6Class(
# 	classname = "Task",
# 	public = list(
# 	  _A = NULL,
# 	  _URI = NULL,
# 	  initialize = function(run: Lazy<Promise<A>>) {}
# 	map<B>(f: (a: A) => B): Task<B> {
# 		return new Task(() => this.run().then(f))
# 	}
# 	ap<B>(fab: Task<(a: A) => B>): Task<B> {
# 		return new Task(() => Promise.all([fab.run(), this.run()]).then(([f, a]) => f(a)))
# 	}
# 	ap_<B, C>(this: Task<(b: B) => C>, fb: Task<B>): Task<C> {
# 		return fb.ap(this)
# 	}
# 	/**
# 		* Combine two effectful actions, keeping only the result of the first
# 	* @since 1.6.0
# 	*/
# 		applyFirst<B>(fb: Task<B>): Task<A> {
# 			return fb.ap(this.map(constant))
# 		}
# 	/**
# 		* Combine two effectful actions, keeping only the result of the second
# 	* @since 1.5.0
# 	*/
# 		applySecond<B>(fb: Task<B>): Task<B> {
# 			return fb.ap(this.map(constIdentity as () => (b: B) => B))
# 		}
# 	chain<B>(f: (a: A) => Task<B>): Task<B> {
# 		return new Task(() => this.run().then(a => f(a).run()))
# 	}
# 	inspect(): string {
# 		return this.toString()
# 	}
# 	toString(): string {
# 		return `new Task(${toString(this.run)})`
# 	}
# }
