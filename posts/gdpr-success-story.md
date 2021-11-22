---
title: "One weird trick to get machine-readable data out of any provider"
date: 2021-05-10
tags: [post, short]
---

**TL;DR:** If you live in the EU or are a EU citizen, companies _must_ provide personal data you provided or they collected in a
machine-readable format, following the article 20 of the GDPR.

Since losing some documents that I really wish I had not lost, I've
been a bit obsessive about preserving all personal data in formats that I control.

Apart from the usual backing up, this includes downloading all my mail even if I use the
web interface to Gmail day-to-day, backing up WhatsApp data personally (this one is a real
pain), and so on.

Recently I wanted to archive my thousands of to-do items. My to-do provider of choice gives a way
to export currently active items, but not completed ones.

However, [the article 20 of the GDPR](https://gdpr-info.eu/art-20-gdpr/) (probably my favorite
of the bunch), states:

> The data subject shall have the right to receive the personal data concerning him or her, which he or she has provided to a controller, in a structured, commonly used and machine-readable format and have the right to transmit those data to another controller without hindrance from the controller to which the personal data have been provided [...]

This led to the following email exchange:

> Hi,
>
> I know about your backup feature, but that only exports current tasks. Is there a way to backup _all_ tasks, also completed ones?
>
> Thanks, Francesco

> Hi Francesco,
>
> Thank you for your message.
>
> Currently, it is not possible to include completed tasks in a backup file. We will share your feedback with our team for future improvements.🙏
>
> Best regards,
> \<support rep name\>

> Hi \<support rep name\>,
>
> I believe that the GDPR requires \<to-do company name\> to provide me with my data in a machine-readable format -- see <https://gdpr-info.eu/art-20-gdpr/>.
>
> I'm not in a hurry, but please provide a way to export my data.
>
> Thanks,
> Francesco

> Hi Francesco,
>
> Thank you for your message.
>
> We have requested your GDPR file. It should be sent to your registered email address within 60 minutes.
>
> If you have any questions, please let us know.
>
> Best regards,
> \<support rep name\>

Lo and behold, a neat JSON dump of all my data appeared in my inbox a few minutes later. The whole affair took maybe
15 minutes on my part.

So, if you live in the EU, or if you are a EU citizen living abroad, know that you should always be always able
to get personal data you provided or they collected by invoking the article 20.
