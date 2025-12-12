<script lang="ts">
	import './layout.css';
	import favicon from '$lib/assets/favicon.svg';

	let { children } = $props();
</script>

<svelte:head><link rel="icon" href={favicon} /></svelte:head>
<div class="navbar bg-base-100 shadow-sm">
  <div class="navbar-start">
    <div class="dropdown">
      <div tabindex="0" role="button" class="btn btn-ghost lg:hidden">
        <svg xmlns="http://www.w3.org/2000/svg" class="h-5 w-5" fill="none" viewBox="0 0 24 24" stroke="currentColor"> <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M4 6h16M4 12h8m-8 6h16" /> </svg>
      </div>
      <ul
        tabindex="-1"
        class="menu menu-sm dropdown-content bg-base-100 rounded-box z-1 mt-3 w-52 p-2 shadow">
      <li><a href="/about">About</a></li>
      <li><a href="/search/brainrnaseq">BrainRNA-seq</a></li>
      <li><a href="/search/string">STRING</a></li>
      <li><a href="/search/ilincs">iLINCS</a></li>
      <li><a href="/search/lookup">Lookup</a></li>
      <li><a href="/search/braincloud">BrainCloud</a></li>
      <li><a href="/search/gtex">GTEx</a></li>
      <li><a href="/search/brainatlas">BrainAtlas</a></li>
      <li><a href="/search/gwas">GWAS</a></li>
      <li><a href="/search/idg">IDG</a></li>
      <li><a href="/search/report">Report</a></li>
    </ul>

    </div>
    <a class="btn btn-ghost text-l mx-6" href="/">Kaleidoscope 2.0</a>
  </div>
  <div class="navbar-center hidden lg:flex">
    <ul class="menu menu-horizontal px-1">
      <li><a href="/about">About</a></li>
      <li><a href="/search/brainrnaseq">BrainRNA-seq</a></li>
      <li><a href="/search/string">STRING</a></li>
      <li><a href="/search/ilincs">iLINCS</a></li>
      <li><a href="/search/lookup">Lookup</a></li>
      <li><a href="/search/braincloud">BrainCloud</a></li>
      <li><a href="/search/gtex">GTEx</a></li>
      <li><a href="/search/brainatlas">BrainAtlas</a></li>
      <li><a href="/search/gwas">GWAS</a></li>
      <li><a href="/search/idg">IDG</a></li>
      <li><a href="/search/report">Report</a></li>
    </ul>
  </div>
  <div class="navbar-end">
    <a class="btn">Button</a>
  </div>
</div>
{@render children()}
