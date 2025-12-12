<div class="min-h-screen bg-base-100 py-12 px-4 sm:px-6 lg:px-8">
  <div class="max-w-4xl mx-auto">
    <div class="card bg-base-200 shadow-xl">
      <div class="card-body p-8 md:p-12">
        
        <article class="prose lg:prose-xl prose-stone max-w-none">
          
          <header class="not-prose mb-8">
            <h1 class="text-4xl md:text-5xl font-bold text-primary mb-4">
              About Kaleidoscope
            </h1>
            <p class="text-xl text-base-content/70 italic leading-relaxed">
              A high-performance gateway for exploring bioinformatics data 
              at the speed of thought.
            </p>
          </header>

          <div class="divider"></div>

          <div class="alert bg-base-300 border-l-4 border-primary italic my-8">
              <div>
                <p>
                  "Science is built up of facts, as a house is built of stones; but an accumulation of facts is no more a science than a heap of stones is a house." 
                  <br />
                  <span class="font-bold text-sm not-italic">— Henri Poincaré</span>
                </p>
              </div>
            </div>

          <section>
            <h2 class="text-secondary">Our Mission</h2>
            <p>
              The volume of genomic data is growing faster than our ability to query it. This portal 
              was built to bridge the gap between databases and interactive 
              analysis. By leveraging modern database technology, we provide ultra-fast 
              lookups of genes.
            </p>
          </section>

          <section>
            <h2 class="text-secondary">The Technology Stack</h2>
            <p>
              To ensure this tool remains fast, secure, and cost-effective, we utilize a 
              cutting-edge "Serverless-at-Edge" architecture:
            </p>
            <ul class="list-none p-0">
              <li class="flex items-start gap-3 mb-4">
                <span class="badge badge-primary mt-1">Backend</span>
                <span>
                  <strong>Rust & DuckDB:</strong> Powered by a high-concurrency 
                  Rust API that queries a compressed DuckDB analytical database.
                </span>
              </li>
              <li class="flex items-start gap-3 mb-4">
                <span class="badge badge-secondary mt-1">Frontend</span>
                <span>
                  <strong>SvelteKit & daisyUI:</strong> Reactive components 
                  providing a "single-page app" feel with accessible, themed UI elements.
                </span>
              </li>
              <li class="flex items-start gap-3">
                <span class="badge badge-accent mt-1">Styling</span>
                <span>
                  <strong>Tailwind Typography:</strong> Ensures optimal vertical rhythm 
                  and readability for scientific documentation.
                </span>
              </li>
            </ul>
          </section>

          <section>
            <h3 class="text-base-content">Data Integrity & Privacy</h3>
            <p>
              All genomic data is stored in an immutable, read-only format. We use 
              server-to-server authentication to ensure that only authorized 
              frontend instances can access the underlying data API.
            </p>
          </section>

          <section>
            <h2 class="text-secondary">Usage & Licensing</h2>
            <p>
              The data presented here is for research purposes. If you use figures generated 
              by this portal in your publications, please cite the original study linked 
              in the dataset metadata section.
            </p>
          </section>

          <footer class="mt-12 not-prose flex justify-center">
            <a href="/" class="btn btn-primary btn-wide group">
              <svg xmlns="http://www.w3.org/2000/svg" class="h-5 w-5 mr-2 group-hover:-translate-x-1 transition-transform" fill="none" viewBox="0 0 24 24" stroke="currentColor">
                <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M10 19l-7-7m0 0l7-7m-7 7h18" />
              </svg>
              Return to Gene Search
            </a>
          </footer>

        </article>
      </div>
    </div>
  </div>
</div>
