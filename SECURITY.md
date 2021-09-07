<!--
     Copyright 2021, Data61, CSIRO (ABN 41 687 119 230)

     SPDX-License-Identifier: CC-BY-SA-4.0
-->

# Vulnerability disclosure policy

Security is a core value of the seL4 Foundation. If you believe you have
found a security vulnerability in seL4, we ask you to work with us to
resolve it according to principles of responsible disclosure. This
policy outlines what we ask of you, and what you can expect of us.

## Scope

This policy currently applies to the most recent released versions, and
the heads of the default branches of the software in the following seL4
Foundation repositories:
- https://github.com/seL4/seL4
- https://github.com/seL4/capdl
- https://github.com/seL4/camkes-tool

Note that these repositories include various code generation tools, for
example the seL4 [bitfield generator], the [capDL-tool] and the
[camkes-tool]. This policy applies to the code these tools ultimately
generate, but not to the code generators themselves. In other words,
it's the code that ends up running on seL4-based systems that we care
about.

[bitfield generator]: https://github.com/seL4/seL4/blob/master/tools/bitfield_gen.py
[capDL-tool]: https://github.com/seL4/capdl/tree/master/capDL-tool
[camkes-tool]: https://github.com/seL4/camkes-tool

## What we ask of you

- Report any vulnerabilities you discover as soon as you can, using the
  official channel below.
- Provide enough detail to allow us to understand the vulnerability and
  its impact.
- Be patient with us if we have questions.
- Allow us reasonable time to fix the vulnerability before you disclose
  it publicly.
- Be mindful that updating the proofs for a fix might take longer than
  you imagine.
- Avoid violating the privacy of others, disrupting the normal operation
  of our systems, or destroying data.

## What you can expect of us

- We will respond to your report within 14 days, and usually sooner.
- We will work with you to understand and reproduce the vulnerability
  you are reporting.
- We will try to reach agreement on an appropriate timeframe for fixing
  the vulnerability. We will usually aim for 90 days, but sometimes we
  will need significantly longer to complete difficult proofs.
- We will work to fix the vulnerability in a timely manner, and will
  keep you informed of our progress.
- When we have developed a fix, we will publicly acknowledge the
  vulnerability.
- If you agree, we will publicly acknowledge your role in finding and
  helping us fix the vulnerability.
- We will protect your privacy, and will not disclose your personally
  identifying information to third parties without your explicit
  permission.

Unfortunately, we are not currently able to offer bug bounties.

## Official channel

Please send vulnerability reports by email to security@seL4.systems,
optionally encrypted using our security officer's [OpenPGP key].

[OpenPGP key]: https://seL4.systems/security.asc

## FAQ

### How can formally verified software contain security vulnerabilities?

seL4 supports many configurations, and only some of these are verified.
For unverified configurations, we want to know about vulnerabilities, so
we can provide the level of support that you would expect from any
well-engineered software.

Even verified configurations contain some code that is assumed to do the
right thing, including boot code, assembly stubs, and cache management.
If that code does the wrong thing, there might be a security
vulnerability in a verified configuration, and we want to know about
that.

The proofs also make a number of assumptions about the way hardware
works. If our assumptions are not valid for real hardware, there might
be a security vulnerability, and we want to know so we can try to fix
our assumptions. Some assumptions might not be possible to fix. For
example, the assumption that memory is incorruptible may be violated by
Rowhammer, but it is quite fundamental to the proofs.

For more information about what the proofs mean, see the [seL4 FAQ].

[seL4 FAQ]: https://docs.sel4.systems/projects/sel4/frequently-asked-questions.html
